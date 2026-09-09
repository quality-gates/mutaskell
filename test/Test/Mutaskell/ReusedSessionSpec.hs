{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Equivalence and compatibility tests for the session-reuse prototype of
-- issue #36.
--
-- Seams under test: the production evaluator 'evalMutant' and the prototype
-- entry points 'evalMutantWithPolicy' \/ 'evalOrderedTests' \/ 'detectPkgEnv'.
--
-- Most cases run one fixture three ways, from the same source and the same
-- ordered test list:
--
--   * @prod@   — the production evaluator (one hint session per test)
--   * @fresh@  — the prototype's instrumented 'FreshPerTest' policy
--   * @reused@ — the prototype's 'LoadOncePerMutant' policy
--
-- @prod@ and @fresh@ must agree everywhere.  Where @reused@ differs, the
-- difference is asserted explicitly; those differences are the compatibility
-- evidence reported on issue #36.
--
-- Known reuse divergence this suite cannot pin: with an exception-catching
-- adapter like AssertCheck, production delivers the timeout inside the
-- interpreted action, so the runner records a plain failure (@killed@),
-- while the reused policy applies the timeout in the driving thread and
-- records an @error@ — and its teardown 'killThread' would be swallowed by
-- the same handler, hanging the session.  A case for it would therefore
-- deadlock the suite; the divergence is recorded in the prototype's module
-- header and on the issue instead.
--
-- Three fixtures cannot run through the production evaluator here: the
-- fixture-with-imported-module case needs a helper module on the interpreter
-- search path (which requires a working directory without the package
-- environment file that production re-detects per test); the timeout case
-- needs a catch-free adapter, because the AssertCheck adapter's 'withCheck'
-- catches 'SomeException' and swallows the timeout's asynchronous exception
-- into a plain test failure (production classifies those kills the same way,
-- so the custom adapter only changes how the kill is observed); and the
-- uncaught-runtime-exception case asserts that production propagates the
-- exception, which the prototype's fresh policy is asserted to match.  The
-- fresh policy stands in for production there; its parity with production is
-- asserted in every other case.
module Test.Mutaskell.ReusedSessionSpec (main, spec) where

import Control.Exception (SomeException, evaluate, try)
import Data.Typeable (Typeable)
import Data.List (isPrefixOf)
import qualified Language.Haskell.Interpreter as I
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (withCurrentDirectory, getCurrentDirectory, listDirectory)
import System.Environment (withArgs)
import System.IO (hClose, stderr, stdout)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Test.Mutaskell.Interpreter (MutantSummary (..), evalMutant, mutantPaths, summarizeResults)
import Test.Mutaskell.Interpreter.ReusedSession
import Test.Mutaskell.TestAdapter
import Test.Mutaskell.Config (MuVar (..))
import Test.Mutaskell.TestAdapter.AssertCheck (AssertStatus (..))
import Test.Mutaskell.TestAdapter.AssertCheckAdapter (AssertCheckRun (..))
import Trace.Hpc.Util (toHpcPos)

main :: IO ()
main = hspec spec

-- ---------------------------------------------------------------- fixtures

-- | A fixture module whose tests pass.
survivorSrc :: String
survivorSrc = unlines
    [ "module MutSurvivor where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "inc :: Int -> Int"
    , "inc n = n + 1"
    , "test_pass1 = assertCheck (inc 1 == 2)"
    , "test_pass2 = assertCheck (inc 2 == 3)"
    ]

-- | A fixture module whose first test fails.
earlyKillSrc :: String
earlyKillSrc = unlines
    [ "module MutEarlyKill where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "inc :: Int -> Int"
    , "inc n = n + 1"
    , "test_fail1 = assertCheck (inc 1 == 99)"
    , "test_pass1 = assertCheck (inc 2 == 3)"
    ]

-- | A fixture module whose second test fails.  The failing test also prints
-- a marker on stderr (unbuffered, so it reliably lands in the captured log)
-- to keep the log-parity assertion from degenerating into empty strings.
-- The AssertCheck test values are pure, so the marker goes through
-- @unsafePerformIO@, evaluated when the test expression is forced.
lateKillSrc :: String
lateKillSrc = unlines
    [ "module MutLateKill where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "import System.IO (hPutStrLn, stderr)"
    , "import System.IO.Unsafe (unsafePerformIO)"
    , "inc :: Int -> Int"
    , "inc n = n + 1"
    , "test_pass1 = assertCheck (inc 1 == 2)"
    , "test_fail2 = unsafePerformIO $ do"
    , "  hPutStrLn stderr \"late-kill-mutant-output\""
    , "  return (assertCheck (inc 2 == 99))"
    ]

-- | A fixture module that does not compile.
invalidSrc :: String
invalidSrc = unlines
    [ "module MutInvalid where"
    , "inc n = n +"
    ]

-- | A fixture module with top-level mutable state of its own.
localStateSrc :: String
localStateSrc = unlines
    [ "module MutStateLocal where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "import Data.IORef"
    , "import System.IO.Unsafe (unsafePerformIO)"
    , "counter :: IORef Int"
    , "counter = unsafePerformIO (newIORef 0)"
    , "tick :: Int"
    , "tick = unsafePerformIO $ atomicModifyIORef' counter (\\c -> (c + 1, c))"
    , "test_state1 = assertCheck (tick == 0)"
    , "test_state2 = assertCheck (tick == 0)"
    ]

-- | A second mutant of the same stateful module (a variant of
-- 'localStateSrc'), used to check state leaking across mutant boundaries.
localStateVariantSrc :: String
localStateVariantSrc = unlines
    [ "module MutStateLocal where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "import Data.IORef"
    , "import System.IO.Unsafe (unsafePerformIO)"
    , "-- variant of the local-state fixture"
    , "counter :: IORef Int"
    , "counter = unsafePerformIO (newIORef 0)"
    , "tick :: Int"
    , "tick = unsafePerformIO $ atomicModifyIORef' counter (\\c -> (c + 1, c))"
    , "test_state1 = assertCheck (tick == 0)"
    , "test_state2 = assertCheck (tick == 0)"
    ]

-- | A fixture module whose mutable state lives in an imported module. The
-- helper is expected in the current directory when the fixture is used.
importStateSrc :: String
importStateSrc = unlines
    [ "module MutStateImport where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "import System.IO.Unsafe (unsafePerformIO)"
    , "import StateHelper"
    , "test_import1 = assertCheck (unsafePerformIO bump == 0)"
    , "test_import2 = assertCheck (unsafePerformIO bump == 0)"
    ]

-- | The imported mutable-state helper for 'importStateSrc'.
stateHelperSrc :: String
stateHelperSrc = unlines
    [ "module StateHelper where"
    , "import Data.IORef"
    , "import System.IO.Unsafe (unsafePerformIO)"
    , "counter :: IORef Int"
    , "counter = unsafePerformIO (newIORef 0)"
    , "bump :: IO Int"
    , "bump = atomicModifyIORef' counter (\\c -> (c + 1, c))"
    ]

-- | A fixture module whose tests touch process-external state: the marker
-- file named by the first process argument.  Each test is an @IO Bool@ action
-- (run through the custom adapter), so the file is read afresh on every
-- invocation and no interpreted-language caching (top-level CAF
-- re-evaluation, @unsafePerformIO@ sharing) can mask or fake it.  Passing the
-- marker path as an extra test argument keeps the state inside a scratch
-- directory instead of the working directory.
externalStateSrc :: String
externalStateSrc = unlines
    [ "module MutStateExternal where"
    , "import System.Directory (doesFileExist)"
    , "import System.Environment (getArgs)"
    , "checkFirst :: IO Bool"
    , "checkFirst = do"
    , "  (marker:_) <- getArgs"
    , "  ex <- doesFileExist marker"
    , "  if ex then return False else writeFile marker \"x\" >> return True"
    ]

-- | 'externalStateSrc' is invoked twice with the same test expression: the
-- first invocation creates the marker file, the second one finds it.
externalTests :: [TestStr]
externalTests = ["checkFirst", "checkFirst"]

-- | A fixture module that reads process arguments (extra test arguments).
argsSrc :: String
argsSrc = unlines
    [ "module MutArgs where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "import System.Environment (getArgs)"
    , "import System.IO.Unsafe (unsafePerformIO)"
    , "hasArg :: Bool"
    , "hasArg = unsafePerformIO $ elem \"mutcheck-extra\" <$> getArgs"
    , "test_args = assertCheck hasArg"
    ]

-- | A fixture module for the custom adapter, whose uncaught runtime exception
-- is not caught by any adapter-side handler.
boomSrc :: String
boomSrc = unlines
    [ "module MutBoolBoom where"
    , "import Control.Exception (throwIO)"
    , "ok :: IO Bool"
    , "ok = pure True"
    , "boom :: IO Bool"
    , "boom = throwIO (userError \"boom\")"
    ]

-- | A custom-adapter fixture whose only test spins long enough to be cut off
-- by a per-test timeout.  It returns @IO Bool@ so no adapter-side handler can
-- catch the timeout's asynchronous exception (the AssertCheck adapter's
-- @withCheck@ catches @SomeException@ and would swallow it into a plain test
-- failure; that behaviour is noted on issue #36 instead of tested here).
boolSpinSrc :: String
boolSpinSrc = unlines
    [ "module MutBoolSpin where"
    , "import Control.Concurrent (threadDelay)"
    , "spinSlow :: IO Bool"
    , "spinSlow = threadDelay 10000000 >> pure True"
    ]

boolSpinTests :: [TestStr]
boolSpinTests = ["spinSlow"]

-- ---------------------------------------------------------- custom adapter

-- | A minimal custom adapter and result type, exercised through the public
-- 'Summarizable' and 'TRun' interfaces. The interpreted test expressions
-- return plain @Bool@ (a library type, so the compiled and interpreted type
-- witnesses agree).
newtype CustRun = CustRun String

instance Summarizable Bool where
    testSummary _ _ result = Summary (_ioLog result)
    isSuccess = id
    isFailure = not
    isOther _ = False

instance TRun CustRun Bool where
    genTest _ t = t
    getName _ = "cust"
    toRun = const (CustRun "fixture")
    summarize_ _ _ _ result = Summary (_ioLog result)
    success_ _ = isSuccess
    failure_ _ = isFailure
    other_ _ = isOther

-- --------------------------------------------------------------- plumbing

-- | Build a mutant value for a fixture source.
mkMutant :: String -> Mutant
mkMutant src = toMutant (MutateValues, toHpcPos (1, 1, 1, 1), src)

-- | Test strings for the AssertCheck adapter fixtures: the survivor module
-- (also reused by fixtures without their own tests).
assertTests :: [TestStr]
assertTests = ["assertCheckResult " ++ t | t <- ["test_pass1", "test_pass2"]]

-- | Test strings for 'earlyKillSrc'.
earlyKillTests :: [TestStr]
earlyKillTests = ["assertCheckResult " ++ t | t <- ["test_fail1", "test_pass1"]]

-- | Test strings for 'lateKillSrc'.
lateKillTests :: [TestStr]
lateKillTests = ["assertCheckResult " ++ t | t <- ["test_pass1", "test_fail2"]]

-- | Test strings for the mutable-state fixtures.
stateTests :: [TestStr]
stateTests = ["assertCheckResult " ++ t | t <- ["test_state1", "test_state2"]]

-- | Test strings for 'importStateSrc'.
importTests :: [TestStr]
importTests = ["assertCheckResult " ++ t | t <- ["test_import1", "test_import2"]]

-- | Test strings for the custom-adapter fixture.
boolTests :: [TestStr]
boolTests = ["ok", "boom"]

assertAdapter :: AssertCheckRun
assertAdapter = AssertCheckRun "fixture"

-- | Classify a single test outcome: @pass@, @fail@, or @error@.
outcome :: Summarizable s => InterpreterOutput s -> String
outcome r = case _io r of
    Left _  -> "error"
    Right x | isSuccess x -> "pass"
            | otherwise   -> "fail"

-- | The outcome sequence of a run.
outcomes :: Summarizable s => [InterpreterOutput s] -> [String]
outcomes = map outcome

-- | The 'MutantSummary' classification of a run.
classification :: (Summarizable s, TRun a s) => a -> [TestStr] -> Mutant -> [InterpreterOutput s] -> String
classification adapter tests mutant results = case summarizeResults adapter tests (mutant, results) of
    MSumError {}  -> "error"
    MSumSkipped{} -> "skipped"
    MSumAlive {}  -> "alive"
    MSumKilled {} -> "killed"
    MSumOther {}  -> "other"

-- | Run one fixture under the production evaluator and under both prototype
-- policies, in a scratch directory, with the same ordered tests. The working
-- directory is left alone, so the production evaluator re-detects the package
-- environment per test exactly as it does in real runs, and the prototype
-- policies use the cache a caller computes once for the run.
runTriplet ::
    (Typeable s, Summarizable s) =>
    [String] -> [TestStr] -> String ->
    IO ( [InterpreterOutput s]
       , (ReusedStats, [InterpreterOutput s])
       , (ReusedStats, [InterpreterOutput s]) )
runTriplet extraArgs tests src = do
    pkgEnv <- detectPkgEnv
    withSystemTempDirectory "mutcheck-reused" $ \dir -> do
        prod <- evalMutant Nothing False dir extraArgs tests (mkMutant src)
        fresh <- evalMutantWithPolicy Nothing False dir pkgEnv extraArgs FreshPerTest tests (mkMutant src)
        reused <- evalMutantWithPolicy Nothing False dir pkgEnv extraArgs LoadOncePerMutant tests (mkMutant src)
        return (prod, fresh, reused)

-- | 'runTriplet' at the AssertCheck adapter's result type.
runAssertTriplet ::
    [String] -> [TestStr] -> String ->
    IO ( [InterpreterOutput AssertStatus]
       , (ReusedStats, [InterpreterOutput AssertStatus])
       , (ReusedStats, [InterpreterOutput AssertStatus]) )
runAssertTriplet = runTriplet

-- | 'runTriplet' at the custom adapter's result type.
runBoolTriplet ::
    [String] -> [TestStr] -> String ->
    IO ( [InterpreterOutput Bool]
       , (ReusedStats, [InterpreterOutput Bool])
       , (ReusedStats, [InterpreterOutput Bool]) )
runBoolTriplet = runTriplet

-- | Run one fixture under a single policy in a scratch directory.
runPolicy ::
    (Typeable s, Summarizable s) =>
    SessionPolicy -> Maybe Int -> [String] -> [TestStr] -> String ->
    IO (ReusedStats, [InterpreterOutput s])
runPolicy policy mtimeout extraArgs tests src = do
    pkgEnv <- detectPkgEnv
    withSystemTempDirectory "mutcheck-reused" $ \dir ->
        evalMutantWithPolicy mtimeout False dir pkgEnv extraArgs policy tests (mkMutant src)

-- | Run one fixture under both prototype policies inside a scratch directory
-- that also holds the helper module the fixture imports.
runInHelperDir ::
    String -> [TestStr] -> String ->
    IO ( (ReusedStats, [InterpreterOutput AssertStatus])
       , (ReusedStats, [InterpreterOutput AssertStatus]) )
runInHelperDir helperSrc tests src = do
    pkgEnv <- detectPkgEnv
    withSystemTempDirectory "mutcheck-stateful" $ \tmp -> do
        writeFile (tmp ++ "/StateHelper.hs") helperSrc
        withCurrentDirectory tmp $ do
            fresh <- evalMutantWithPolicy Nothing False tmp pkgEnv [] FreshPerTest tests (mkMutant src)
            reused <- evalMutantWithPolicy Nothing False tmp pkgEnv [] LoadOncePerMutant tests (mkMutant src)
            return (fresh, reused)

-- | Assert the production evaluator and the instrumented fresh policy agree,
-- and that the fresh policy attempted exactly one session and load per test.
expectFreshParity ::
    (Summarizable s, TRun a s) => a -> [TestStr] -> Mutant ->
    [InterpreterOutput s] -> (ReusedStats, [InterpreterOutput s]) -> Expectation
expectFreshParity adapter tests mutant prod (stats, fresh) = do
    outcomes fresh `shouldBe` outcomes prod
    classification adapter tests mutant fresh `shouldBe` classification adapter tests mutant prod
    -- one hint session and one module load per attempted test
    rsSessions stats `shouldBe` length fresh
    rsLoads stats `shouldBe` length fresh
    rsTestsRun stats `shouldBe` length fresh

spec :: Spec
spec = describe "ReusedSession" $ do

    describe "run-scoped package-environment cache" $
        it "returns the environment file of the current directory when present" $ do
            pkgEnv <- detectPkgEnv
            cwd <- getCurrentDirectory
            entries <- listDirectory cwd
            let envFiles = filter (".ghc.environment." `isPrefixOf`) entries
            case envFiles of
                []     -> pkgEnvArgs pkgEnv `shouldBe` []
                (f:_)  -> pkgEnvArgs pkgEnv `shouldBe` ["-package-env", cwd ++ "/" ++ f]

    describe "empty test list" $
        it "starts no session under either policy" $ do
            (stats, results) <- runPolicy FreshPerTest Nothing [] [] survivorSrc :: IO (ReusedStats, [InterpreterOutput AssertStatus])
            outcomes results `shouldBe` []
            stats `shouldBe` emptyStats
            (rstats, rresults) <- runPolicy LoadOncePerMutant Nothing [] [] survivorSrc :: IO (ReusedStats, [InterpreterOutput AssertStatus])
            outcomes rresults `shouldBe` []
            rstats `shouldBe` emptyStats

    describe "survivor" $
        it "runs every test in order under every policy" $ do
            (prod, fresh, reused) <- runAssertTriplet [] assertTests survivorSrc
            outcomes prod `shouldBe` ["pass", "pass"]
            expectFreshParity assertAdapter assertTests (mkMutant survivorSrc) prod fresh
            outcomes (snd reused) `shouldBe` ["pass", "pass"]
            classification assertAdapter assertTests (mkMutant survivorSrc) (snd reused) `shouldBe` "alive"
            let (stats, _) = reused
            rsSessions stats `shouldBe` 1
            rsLoads stats `shouldBe` 1
            rsTestsRun stats `shouldBe` 2
            -- instrumentation separates setup from test time
            rsSetupNs stats `shouldSatisfy` (> 0)
            rsTestNs stats `shouldSatisfy` (> 0)

    describe "early kill" $
        it "stops at the first failure under every policy" $ do
            (prod, fresh, reused) <- runAssertTriplet [] earlyKillTests earlyKillSrc
            outcomes prod `shouldBe` ["fail"]
            expectFreshParity assertAdapter earlyKillTests (mkMutant earlyKillSrc) prod fresh
            outcomes (snd reused) `shouldBe` ["fail"]
            let (stats, _) = reused
            rsSessions stats `shouldBe` 1
            rsTestsRun stats `shouldBe` 1

    describe "late kill" $ do
        it "runs both tests and stops at the second failure" $ do
            (prod, fresh, reused) <- runAssertTriplet [] lateKillTests lateKillSrc
            outcomes prod `shouldBe` ["pass", "fail"]
            expectFreshParity assertAdapter lateKillTests (mkMutant lateKillSrc) prod fresh
            outcomes (snd reused) `shouldBe` ["pass", "fail"]
            classification assertAdapter lateKillTests (mkMutant lateKillSrc) (snd reused) `shouldBe` "killed"
            let (stats, _) = reused
            rsSessions stats `shouldBe` 1
            rsLoads stats `shouldBe` 1
            rsTestsRun stats `shouldBe` 2
        it "leaves the same test log behind under both prototype policies" $ do
            pkgEnv <- detectPkgEnv
            withSystemTempDirectory "mutcheck-reused" $ \dir -> do
                freshLog <- do
                    _ <- evalMutantWithPolicy Nothing False dir pkgEnv [] FreshPerTest lateKillTests (mkMutant lateKillSrc)
                        :: IO (ReusedStats, [InterpreterOutput AssertStatus])
                    readLog dir (mkMutant lateKillSrc)
                reusedLog <- do
                    _ <- evalMutantWithPolicy Nothing False dir pkgEnv [] LoadOncePerMutant lateKillTests (mkMutant lateKillSrc)
                        :: IO (ReusedStats, [InterpreterOutput AssertStatus])
                    readLog dir (mkMutant lateKillSrc)
                -- The failing test's marker keeps this from pinning empty
                -- strings against each other.
                lines freshLog `shouldSatisfy` elem "late-kill-mutant-output"
                lines reusedLog `shouldBe` lines freshLog

    describe "invalid mutant" $
        it "is skipped under every policy" $ do
            (prod, fresh, reused) <- runAssertTriplet [] assertTests invalidSrc
            outcomes prod `shouldBe` ["error"]
            classification assertAdapter assertTests (mkMutant invalidSrc) prod `shouldBe` "skipped"
            expectFreshParity assertAdapter assertTests (mkMutant invalidSrc) prod fresh
            outcomes (snd reused) `shouldBe` ["error"]
            classification assertAdapter assertTests (mkMutant invalidSrc) (snd reused) `shouldBe` "skipped"
            let (stats, _) = reused
            rsSessions stats `shouldBe` 1
            rsLoads stats `shouldBe` 1
            rsTestsRun stats `shouldBe` 1

    describe "extra test arguments" $
        it "are visible to every test under every policy" $ do
            (prod, fresh, reused) <-
                withArgs ["mutcheck-extra"] (runAssertTriplet ["mutcheck-extra"] ["assertCheckResult test_args"] argsSrc)
            outcomes prod `shouldBe` ["pass"]
            expectFreshParity assertAdapter ["assertCheckResult test_args"] (mkMutant argsSrc) prod fresh
            outcomes (snd reused) `shouldBe` ["pass"]

    describe "top-level mutable state in the module under test" $
        it "is re-evaluated per interpreted expression, so every policy agrees" $ do
            (prod, fresh, reused) <- runAssertTriplet [] stateTests localStateSrc
            outcomes prod `shouldBe` ["pass", "pass"]
            expectFreshParity assertAdapter stateTests (mkMutant localStateSrc) prod fresh
            -- GHCi re-evaluates the top-level CAFs of loaded modules when it
            -- interprets a new expression, so even the reused session starts
            -- every test from tick == 0.  State that survives reuse only
            -- appears where it is created inside an IO action that is not a
            -- top-level CAF of the loaded module (the imported-module case).
            outcomes (snd reused) `shouldBe` ["pass", "pass"]
            classification assertAdapter stateTests (mkMutant localStateSrc) (snd reused) `shouldBe` "alive"

    describe "mutable state in an imported module" $
        it "is retained across tests by the reused session" $ do
            (fresh, reused) <- runInHelperDir stateHelperSrc importTests importStateSrc
            outcomes (snd fresh) `shouldBe` ["pass", "pass"]
            -- the reused session keeps the imported module's state loaded
            outcomes (snd reused) `shouldBe` ["pass", "fail"]

    describe "interpreter-owned state across mutants" $
        it "does not leak from one mutant's session into the next" $ do
            pkgEnv <- detectPkgEnv
            withSystemTempDirectory "mutcheck-reused" $ \dir -> do
                (statsA, resultsA) <- evalMutantWithPolicy Nothing False dir pkgEnv [] LoadOncePerMutant
                    stateTests (mkMutant localStateSrc)
                    :: IO (ReusedStats, [InterpreterOutput AssertStatus])
                outcomes resultsA `shouldBe` ["pass", "pass"]
                rsSessions statsA `shouldBe` 1
                (statsB, resultsB) <- evalMutantWithPolicy Nothing False dir pkgEnv [] LoadOncePerMutant
                    stateTests (mkMutant localStateVariantSrc)
                    :: IO (ReusedStats, [InterpreterOutput AssertStatus])
                -- the next mutant starts in its own session, so nothing the
                -- first mutant's session computed is visible to it
                outcomes resultsB `shouldBe` ["pass", "pass"]
                rsSessions statsB `shouldBe` 1

    describe "process-external state" $ do
        it "behaves the same under every policy (never reset)" $ do
            pkgEnv <- detectPkgEnv
            withSystemTempDirectory "mutcheck-external" $ \dir -> do
                -- Each policy gets its own marker so its state starts absent;
                -- the first invocation creates it, the second one finds it.
                let markerFor name = dir ++ "/external-state-" ++ name ++ ".log"
                    prod = withArgs [markerFor "prod"] $
                        evalMutant Nothing False dir [markerFor "prod"] externalTests (mkMutant externalStateSrc)
                            :: IO [InterpreterOutput Bool]
                    fresh = snd <$> withArgs [markerFor "fresh"] (
                        evalMutantWithPolicy Nothing False dir pkgEnv [markerFor "fresh"] FreshPerTest
                            externalTests (mkMutant externalStateSrc)
                            :: IO (ReusedStats, [InterpreterOutput Bool]))
                    reused = snd <$> withArgs [markerFor "reused"] (
                        evalMutantWithPolicy Nothing False dir pkgEnv [markerFor "reused"] LoadOncePerMutant
                            externalTests (mkMutant externalStateSrc)
                            :: IO (ReusedStats, [InterpreterOutput Bool]))
                outcomes <$> prod >>= (`shouldBe` ["pass", "fail"])
                outcomes <$> fresh >>= (`shouldBe` ["pass", "fail"])
                outcomes <$> reused >>= (`shouldBe` ["pass", "fail"])
        it "persists across mutants under both policies" $ do
            pkgEnv <- detectPkgEnv
            withSystemTempDirectory "mutcheck-external" $ \dir -> do
                let marker = dir ++ "/external-state.log"
                    runWith policy = withArgs [marker] $
                        evalMutantWithPolicy Nothing False dir pkgEnv [marker] policy
                            externalTests (mkMutant externalStateSrc)
                            :: IO (ReusedStats, [InterpreterOutput Bool])
                (_, firstFresh) <- runWith FreshPerTest
                outcomes firstFresh `shouldBe` ["pass", "fail"]
                (_, secondFresh) <- runWith FreshPerTest
                outcomes secondFresh `shouldBe` ["fail"]
                (_, firstReused) <- runWith LoadOncePerMutant
                outcomes firstReused `shouldBe` ["fail"]

    describe "timeout" $ do
        it "abandons the test and tears the session down under both policies" $ do
            (fstats, fresults) <- runPolicy FreshPerTest (Just 1000000) [] boolSpinTests boolSpinSrc
                :: IO (ReusedStats, [InterpreterOutput Bool])
            outcomes fresults `shouldBe` ["error"]
            case _io (last fresults) of
                Left (I.UnknownError msg) -> msg `shouldBe` "Timeout occurred"
                _ -> expectationFailure "expected a timeout error"
            rsSessions fstats `shouldBe` 1
            rsTestsRun fstats `shouldBe` 1
            (rstats, rresults) <- runPolicy LoadOncePerMutant (Just 1000000) [] boolSpinTests boolSpinSrc
                :: IO (ReusedStats, [InterpreterOutput Bool])
            outcomes rresults `shouldBe` ["error"]
            case _io (last rresults) of
                Left (I.UnknownError msg) -> msg `shouldBe` "Timeout occurred"
                _ -> expectationFailure "expected a timeout error"
            rsSessions rstats `shouldBe` 1
            rsLoads rstats `shouldBe` 1
            rsTestsRun rstats `shouldBe` 1
        it "leaves the process able to run the next mutant after a timeout" $ do
            pkgEnv <- detectPkgEnv
            withSystemTempDirectory "mutcheck-reused" $ \dir -> do
                _ <- evalMutantWithPolicy (Just 1000000) False dir pkgEnv [] LoadOncePerMutant
                    boolSpinTests (mkMutant boolSpinSrc)
                    :: IO (ReusedStats, [InterpreterOutput Bool])
                (_, after) <- evalMutantWithPolicy Nothing False dir pkgEnv [] LoadOncePerMutant
                    assertTests (mkMutant survivorSrc)
                    :: IO (ReusedStats, [InterpreterOutput AssertStatus])
                outcomes after `shouldBe` ["pass", "pass"]

    describe "uncaught runtime exception (custom adapter)" $ do
        it "propagates out of the production evaluator and the fresh policy" $ do
            -- The production catchOutput restores the process streams only
            -- when a run completes; a run aborted by an exception leaks the
            -- redirection into this test process.  Capture the real streams
            -- and restore them after the run so later output stays visible.
            goodOut <- hDuplicate stdout
            goodErr <- hDuplicate stderr
            prodExc <- withSystemTempDirectory "mutcheck-reused" $ \dir ->
                try (evalMutant Nothing False dir [] boolTests (mkMutant boomSrc))
                    :: IO (Either SomeException [InterpreterOutput Bool])
            hDuplicateTo goodOut stdout
            hDuplicateTo goodErr stderr
            hClose goodOut
            hClose goodErr
            isLeft prodExc `shouldBe` True
            pkgEnv <- detectPkgEnv
            freshExc <- withSystemTempDirectory "mutcheck-reused" $ \dir ->
                try (evalMutantWithPolicy Nothing False dir pkgEnv [] FreshPerTest boolTests (mkMutant boomSrc))
                    :: IO (Either SomeException (ReusedStats, [InterpreterOutput Bool]))
            isLeft freshExc `shouldBe` True
        it "is classified as an error by the reused session" $ do
            (stats, reused) <- runPolicy LoadOncePerMutant Nothing [] boolTests boomSrc
                :: IO (ReusedStats, [InterpreterOutput Bool])
            outcomes reused `shouldBe` ["pass", "error"]
            classification (CustRun "fixture") boolTests (mkMutant boomSrc) reused `shouldBe` "error"
            rsSessions stats `shouldBe` 1
            rsLoads stats `shouldBe` 1
            rsTestsRun stats `shouldBe` 2

-- -------------------------------------------------------------- utilities

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- | The log file of a mutant written under @dir@.
readLog :: FilePath -> Mutant -> IO String
readLog dir mutant = do
    let (_, _, logF) = mutantPaths dir mutant
    -- Force the whole read before returning so the file's handle is closed
    -- (a lazily held thunk keeps the read lock and breaks the next run's
    -- truncating open of the same log).
    contents <- readFile logF
    _ <- evaluate (length contents)
    return contents
