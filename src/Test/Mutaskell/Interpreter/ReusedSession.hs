{-# LANGUAGE ScopedTypeVariables #-}

{- | Prototype for issue #36: reusing one hint session per mutant across its
ordered tests.

The production evaluator ('Test.Mutaskell.evalMutant') starts a fresh hint
session and loads the mutant module once @per test expression@.  This module
provides an instrumented equivalent of that behaviour ('FreshPerTest') and a
single-session variant ('LoadOncePerMutant') that loads the mutant module once
and runs the ordered tests inside the same session, keeping the same ordering,
short-circuiting and per-test timeout rules.

This is an investigation artifact, not production behaviour: neither
'Test.Mutaskell.mucheck' nor the CLI select a policy, and the default
evaluator is unchanged.  Concurrent mutants still require separate processes
(hint is not thread-safe, and hint permits only one session per process at a
time).
-}
module Test.Mutaskell.Interpreter.ReusedSession
    ( -- * Session policies
      SessionPolicy (..)
      -- * Instrumentation
    , ReusedStats (..)
    , emptyStats
      -- * Run-scoped package-environment cache
    , PkgEnvCache (..)
    , detectPkgEnv
      -- * Evaluation entry points
    , evalOrderedTests
    , evalMutantWithPolicy
    ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException (..), bracket, try)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Typeable (Typeable)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Language.Haskell.Interpreter as I
import qualified Language.Haskell.Interpreter.Unsafe as IU
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Environment (lookupEnv, withArgs)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (IOMode (WriteMode), hClose, stderr, stdout, withFile)
import System.Timeout (timeout)

import Test.Mutaskell.Interpreter (findPkgEnvArgs, mutantPaths, parentDir)
import Test.Mutaskell.TestAdapter
import Test.Mutaskell.Utils.Print (say)

-- | Monotonic clock reading in nanoseconds.
nowNs :: IO Integer
nowNs = fromIntegral <$> getMonotonicTimeNSec

-- | How test expressions are mapped onto hint sessions.
data SessionPolicy
    = FreshPerTest
      -- ^ One hint session and one module load per test expression; each
      --   test's timeout covers session boot, module load and the test
      --   itself.  This mirrors the production evaluator
      --   ('Test.Mutaskell.evalTest').
    | LoadOncePerMutant
      -- ^ One hint session per mutant: the session is booted and the module
      --   loaded once, on the first test, and every remaining ordered test
      --   runs inside that session.  With limit @T@, the first visited test's
      --   @T@ covers session boot, module load and the first expression;
      --   every subsequent test's @T@ covers only its own preparation and
      --   execution.  An empty test list starts no session.
    deriving (Eq, Show)

-- | Instrumentation counters and timings for one mutant evaluation.
--
-- @rsTestsRun@ counts test invocations @attempted@: a test whose session fails
-- to load the mutant module (invalid mutant) or that times out has been
-- invoked, so it is counted.
data ReusedStats = ReusedStats
    { rsSessions :: !Int     -- ^ hint sessions started
    , rsLoads    :: !Int     -- ^ 'I.loadModules' calls made
    , rsTestsRun :: !Int     -- ^ test invocations attempted
    , rsSetupNs  :: !Integer -- ^ nanoseconds spent in session boot and module load
    , rsTestNs   :: !Integer -- ^ nanoseconds spent in test preparation and execution
    } deriving (Eq, Show)

-- | Zeroed 'ReusedStats'.
emptyStats :: ReusedStats
emptyStats = ReusedStats 0 0 0 0 0

-- | Run-scoped cache of package-environment discovery.
--
-- The current directory, its @.ghc.environment.*@ files and the build
-- environment they point at are the inputs of the detection, so a cached
-- value is only valid for a run that does not change any of them.  A new run,
-- a change of working directory or a rebuild must re-detect via
-- 'detectPkgEnv'.  The cache is deliberately not process-global and carries no
-- key beyond the run that computed it.
newtype PkgEnvCache = PkgEnvCache { pkgEnvArgs :: [String] }
    deriving (Eq, Show)

-- | Detect the package-environment arguments once per run.
detectPkgEnv :: IO PkgEnvCache
detectPkgEnv = PkgEnvCache <$> findPkgEnvArgs

-- | Run the ordered tests on one mutant under a 'SessionPolicy', stopping at
-- the first test failure, interpreter error or timeout.  The result list is
-- ordered like the input tests, and its last entry is the first
-- failure\/error\/timeout.  An empty test list starts no session.
evalOrderedTests ::
    forall t. (Typeable t, Summarizable t) =>
    -- | Optional per-test timeout in microseconds
    Maybe Int ->
    -- | Extra arguments forwarded to every test invocation via @withArgs@
    [String] ->
    -- | The mutant module file to load
    FilePath ->
    -- | The file to capture test output into
    FilePath ->
    -- | Cached package-environment arguments for this run
    PkgEnvCache ->
    -- | Session policy
    SessionPolicy ->
    -- | The ordered tests to run
    [TestStr] ->
    -- | Instrumentation and the outputs of the tests that ran
    IO (ReusedStats, [InterpreterOutput t])
evalOrderedTests _ _ _ _ _ _ [] = return (emptyStats, [])
evalOrderedTests mtimeout extraArgs mutantFile logF pkgEnv policy tests0@(t0:_) = do
    statsRef <- newIORef emptyStats
    case policy of
        FreshPerTest      -> freshLoop statsRef tests0 []
        LoadOncePerMutant -> reusedLoop statsRef
  where
    -- Counters are shared with the session thread, so updates are atomic.
    bumpSessions r = atomicModifyIORef' r (\s -> (s {rsSessions = rsSessions s + 1}, ()))
    bumpLoads r    = atomicModifyIORef' r (\s -> (s {rsLoads = rsLoads s + 1}, ()))
    bumpTests r    = atomicModifyIORef' r (\s -> (s {rsTestsRun = rsTestsRun s + 1}, ()))
    addSetup r ns  = atomicModifyIORef' r (\s -> (s {rsSetupNs = rsSetupNs s + ns}, ()))
    addTest r ns   = atomicModifyIORef' r (\s -> (s {rsTestNs = rsTestNs s + ns}, ()))

    collect r acc = do
        s <- readIORef r
        return (s, reverse acc)

    -- Classification of an abandoned test.
    timeoutOutput = Io{_io = Left (I.UnknownError "Timeout occurred"), _ioLog = logF}

    -- Fresh policy: sequential tests, each in its own session.
    freshLoop r [] acc = collect r acc
    freshLoop r (t:ts) acc = do
        v <- freshOne r t
        case _io v of
            Left _  -> collect r (v : acc)
            Right out
                | isSuccess out -> freshLoop r ts (v : acc)
                | otherwise     -> collect r (v : acc)

    -- Run one test in its own session: boot, module load and test all fall
    -- inside this test's timeout, matching the production 'evalTest' shape.
    freshOne r t = do
        bumpSessions r
        bumpLoads r
        bumpTests r
        start <- nowNs
        loadEndRef <- newIORef start
        let runAction = withArgs extraArgs $ withCapturedOutput logF $
                IU.unsafeRunInterpreterWithArgs (pkgEnvArgs pkgEnv) $ do
                    bootEnd <- liftIO nowNs
                    liftIO (addSetup r (bootEnd - start))
                    I.loadModules [mutantFile]
                    loadEnd <- liftIO nowNs
                    liftIO (addSetup r (loadEnd - bootEnd))
                    liftIO (writeIORef loadEndRef loadEnd)
                    ms <- I.getLoadedModules
                    I.setTopLevelModules ms
                    interpretTest t
        mval <- applyTimeout mtimeout runAction
        case mval of
            Nothing -> return timeoutOutput
            Just v  -> do
                tEnd <- nowNs
                loadEnd <- readIORef loadEndRef
                addTest r (tEnd - loadEnd)
                return Io{_io = v, _ioLog = logF}

    -- Reused policy: one session, driven over a request\/response channel.
    reusedLoop r = do
        reqMV <- newEmptyMVar
        respMV <- newEmptyMVar
        doneMV <- newEmptyMVar
        tidRef <- newIORef Nothing
        bumpSessions r
        bumpTests r
        -- The session thread is forked inside the first test's timeout so
        -- that boot, module load and the first expression all fall inside the
        -- first test's limit.
        let forkAndSend = do
                tid <- forkIO (sessionThread r reqMV respMV doneMV)
                writeIORef tidRef (Just tid)
                putMVar reqMV (Just t0)
                takeMVar respMV
        mfirst <- case mtimeout of
            Nothing -> Just <$> forkAndSend
            Just t  -> timeout t forkAndSend
        case mfirst of
            Nothing -> do
                tearDown tidRef doneMV
                s <- readIORef r
                return (s, [timeoutOutput])
            Just v0 -> case _io v0 of
                Left _  -> finish r reqMV doneMV [v0]
                Right out
                    | isSuccess out -> nextTests r reqMV respMV doneMV tidRef [v0] (drop 1 tests0)
                    | otherwise     -> finish r reqMV doneMV [v0]

    -- Continue with the remaining tests once the first one passed.
    nextTests r reqMV _respMV doneMV _tidRef acc [] = finish r reqMV doneMV acc
    nextTests r reqMV respMV doneMV tidRef acc (t:ts) = do
        bumpTests r
        putMVar reqMV (Just t)
        mval <- applyTimeout mtimeout (takeMVar respMV)
        case mval of
            Nothing -> do
                tearDown tidRef doneMV
                s <- readIORef r
                return (s, reverse (timeoutOutput : acc))
            Just v -> case _io v of
                Left _  -> finish r reqMV doneMV (v : acc)
                Right out
                    | isSuccess out -> nextTests r reqMV respMV doneMV tidRef (v : acc) ts
                    | otherwise     -> finish r reqMV doneMV (v : acc)

    -- Normal end (all tests done, or stopped at a failure\/error): ask the
    -- session loop to end, then wait for the session to be cleaned up.
    finish r reqMV doneMV acc = do
        _ <- tryPutMVar reqMV Nothing
        _ <- takeMVar doneMV
        collect r acc

    -- Timeout end: the session may be damaged (a test is still running), so
    -- it is never reused; kill it and wait for its cleanup.
    tearDown tidRef doneMV = do
        mtid <- readIORef tidRef
        case mtid of
            Nothing  -> return ()
            Just tid -> killThread tid >> takeMVar doneMV

    -- One hint session per mutant.  The session is booted here; the module is
    -- loaded on the first request, and every further request runs inside the
    -- same session.  Boot, module load and the first expression are therefore
    -- all charged to the first test's timeout, and the session is ended as
    -- soon as a test fails, errors or the driving thread stops asking.
    sessionThread r reqMV respMV doneMV = do
        start <- nowNs
        outcome <- try (IU.unsafeRunInterpreterWithArgs (pkgEnvArgs pkgEnv) (reusedAction start))
            :: IO (Either SomeException (Either I.InterpreterError ()))
        case outcome of
            Right (Right ())        -> return ()
            Right (Left err)        -> deliver (Io{_io = Left err, _ioLog = logF})
            Left (SomeException ex) -> deliver (Io{_io = Left (I.UnknownError (show ex)), _ioLog = logF})
        putMVar doneMV ()
      where
        deliver out = do
            _ <- tryPutMVar respMV out
            return ()

        reusedAction start = do
            bootEnd <- liftIO nowNs
            liftIO (addSetup r (bootEnd - start))
            first <- liftIO (takeMVar reqMV)
            case first of
                Nothing -> return ()
                Just t  -> do
                    liftIO (bumpLoads r)
                    I.loadModules [mutantFile]
                    loadEnd <- liftIO nowNs
                    liftIO (addSetup r (loadEnd - bootEnd))
                    ms <- I.getLoadedModules
                    I.setTopLevelModules ms
                    keep <- runAndRespond t
                    when keep loopTests

        -- Run one test, hand its result to the driving thread, and report
        -- whether the ordered run may continue.
        runAndRespond t = do
            tStart <- liftIO nowNs
            out <- runTestInSession t
            tEnd <- liftIO nowNs
            liftIO (addTest r (tEnd - tStart))
            liftIO (putMVar respMV out)
            return $ case _io out of
                Left _  -> False
                Right o -> isSuccess o

        loopTests = do
            mb <- liftIO (takeMVar reqMV)
            case mb of
                Nothing -> return ()
                Just t  -> do
                    keep <- runAndRespond t
                    when keep loopTests

    -- Run one test expression inside the current session.
    interpretTest :: TestStr -> I.Interpreter t
    interpretTest t = do
        act <- I.interpret t (I.as :: ((Typeable t) => IO t))
        liftIO (withArgs extraArgs (withCapturedOutput logF act))

    runTestInSession :: TestStr -> I.Interpreter (InterpreterOutput t)
    runTestInSession t = do
        v <- interpretTest t
        return Io{_io = Right v, _ioLog = logF}

-- | Apply the optional per-test timeout.
applyTimeout :: Maybe Int -> IO a -> IO (Maybe a)
applyTimeout Nothing action = Just <$> action
applyTimeout (Just t) action = timeout t action

-- | Capture stdout and stderr of one test run into @logF@.
--
-- Unlike the production 'Test.Mutaskell.Utils.Print.catchOutput', the
-- redirection is restored even when the run is abandoned (a timeout teardown
-- kills the session thread mid-run), so a killed session cannot leave the
-- process stdout pointing at a dead log handle.
withCapturedOutput :: FilePath -> IO a -> IO a
withCapturedOutput logF action = do
    isdebug <- lookupEnv "MuDEBUG"
    case isdebug of
        Just _  -> action
        Nothing -> withFile logF WriteMode $ \logH ->
            bracket
                (do stdoutDup <- hDuplicate stdout
                    stderrDup <- hDuplicate stderr
                    hDuplicateTo logH stdout
                    hDuplicateTo logH stderr
                    -- The run keeps the file open through the redirected
                    -- stdout and stderr dups, so the file's write lock is
                    -- released here (as the production 'catchOutput' does);
                    -- otherwise a second run of the same test in this process
                    -- could not open the same log file again.
                    hClose logH
                    return (stdoutDup, stderrDup))
                (\(stdoutDup, stderrDup) -> do
                    hDuplicateTo stdoutDup stdout
                    hDuplicateTo stderrDup stderr
                    hClose stdoutDup
                    hClose stderrDup)
                (const action)

-- | Evaluate one mutant under a 'SessionPolicy': write the mutant file into
-- @mutantDir@, run the ordered tests, and optionally delete the mutant's hash
-- directory afterwards.  The mutant-level counterpart of
-- 'Test.Mutaskell.evalMutant'.
evalMutantWithPolicy ::
    (Typeable t, Summarizable t) =>
    -- | Optional per-test timeout in microseconds
    Maybe Int ->
    -- | Whether to delete the mutant files after evaluation
    Bool ->
    -- | Directory to write the mutant file into
    FilePath ->
    -- | Cached package-environment arguments for this run
    PkgEnvCache ->
    -- | Extra arguments forwarded to every test invocation via @withArgs@
    [String] ->
    -- | Session policy
    SessionPolicy ->
    -- | The ordered tests to run
    [TestStr] ->
    -- | Mutant being tested
    Mutant ->
    -- | Instrumentation and the result of the test runs
    IO (ReusedStats, [InterpreterOutput t])
evalMutantWithPolicy mtimeout doDelete mutantDir pkgEnv extraArgs policy tests mutant = do
    let (hashDir, mutantFile, logF) = mutantPaths mutantDir mutant

    say mutantFile

    createDirectoryIfMissing True (parentDir mutantFile)
    writeResult <- try (writeFile mutantFile (_mutant mutant)) :: IO (Either IOException ())
    (stats, result) <- case writeResult of
        Left err -> return (emptyStats, [Io{_io = Left (I.UnknownError ("write error: " ++ show err)), _ioLog = ""}])
        Right () -> evalOrderedTests mtimeout extraArgs mutantFile logF pkgEnv policy tests
    when doDelete $ do
        _ <- try (removeDirectoryRecursive hashDir) :: IO (Either IOException ())
        return ()
    return (stats, result)