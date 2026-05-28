{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{- | The Interpreter module is responible for invoking the Hint interpreter to
evaluate mutants.
-}
module Test.MuCheck.Interpreter (evaluateMutants, evalMethod, evalMutant, evalTest, summarizeResults, summaryFromMutantSummaries, MutantSummary (..), isSkippedSummary) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Char (isAlphaNum)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, partition)
import Data.Typeable
import qualified Language.Haskell.Interpreter as I
import qualified Language.Haskell.Interpreter.Unsafe as IU
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, listDirectory, removeDirectoryRecursive)
import System.Environment (withArgs)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.Timeout (timeout)

import Test.MuCheck.AnalysisSummary
import Test.MuCheck.TestAdapter
import Test.MuCheck.Utils.Common
import Test.MuCheck.Utils.Print

-- | Data type to hold results of a single test execution
data MutantSummary = MSumError Mutant String [Summary]         -- ^ Interpreter or runtime error
                   | MSumSkipped Mutant [Summary]              -- ^ Non-compilable mutant (WontCompile)
                   | MSumAlive Mutant [Summary]                -- ^ The mutant was alive
                   | MSumKilled Mutant [Summary]               -- ^ The mutant was killed
                   | MSumOther Mutant [Summary]                -- ^ Undetermined - we will treat it as killed as it is not a success.
                   deriving (Show, Read)

-- | True when the summary represents a non-compilable (skipped) mutant.
isSkippedSummary :: MutantSummary -> Bool
isSkippedSummary (MSumSkipped _ _) = True
isSkippedSummary _                 = False

-- | Build an 'MAnalysisSummary' directly from a list of per-mutant results.
-- Use this when results were collected outside the normal 'evaluateMutants' call
-- (e.g. from parallel worker subprocesses).
summaryFromMutantSummaries :: [MutantSummary] -> MAnalysisSummary
summaryFromMutantSummaries sums = MAnalysisSummary
  { _maCoveredNumMutants = -1
  , _maNumMutants        = length sums
  , _maAlive             = length [() | MSumAlive   _ _   <- sums]
  , _maKilled            = length [() | MSumKilled  _ _   <- sums]
                         + length [() | MSumOther   _ _   <- sums]
  , _maErrors            = length [() | MSumError   _ _ _ <- sums]
  , _maSkipped           = length [() | MSumSkipped _ _   <- sums]
  }

-- | Given the list of tests suites to check, run the test suite on mutants.
evaluateMutants ::
    (Show b, Summarizable b, TRun a b) =>
    -- | Number of parallel worker processes
    Int ->
    -- | Optional timeout in microseconds
    Maybe Int ->
    -- | Optional directory to keep mutant files in (Nothing = system temp, deleted after)
    Maybe FilePath ->
    -- | Extra arguments forwarded to every test invocation via @withArgs@
    [String] ->
    -- | Optional per-mutant callback invoked after each mutant is evaluated
    Maybe (MutantSummary -> IO ()) ->
    -- | The module to be evaluated
    a ->
    -- | The mutants to be evaluated
    [Mutant] ->
    -- | The tests to be used for analysis
    [TestStr] ->
    -- | Returns a tuple of full run summary and individual mutant summary
    IO (MAnalysisSummary, [MutantSummary])
evaluateMutants _numWorkers mtimeout keepDir extraArgs mcallback m mutants tests = do
    mutantDir <- resolveMutantDir keepDir
    let doDelete = keepDir == Nothing
        evalOne mutant = do
            result  <- evalMutant mtimeout doDelete mutantDir extraArgs tests mutant
            let summary = summarizeResults m tests (mutant, result)
            case mcallback of
                Nothing -> return ()
                Just cb -> cb summary
            return (result, summary)
    pairs <- mapM evalOne mutants
    let results   = map fst pairs
        summaries = map snd pairs
        ma        = fullSummary m tests results
    return (ma, summaries)

-- | Compute the directory to write mutant files into.
-- If a keep-dir is provided, use it; otherwise use the system temp dir.
resolveMutantDir :: Maybe FilePath -> IO FilePath
resolveMutantDir (Just dir) = createDirectoryIfMissing True dir >> return dir
resolveMutantDir Nothing    = getCanonicalTemporaryDirectory

-- | Extract the module name from the first line of a Haskell source string.
-- Returns @\"Main\"@ if no @module@ declaration is found.
extractModuleName :: String -> String
extractModuleName src =
    case dropWhile (not . ("module " `isPrefixOf`)) (lines src) of
        []    -> "Main"
        (l:_) -> let rest = drop (length "module ") l
                     name = takeWhile (\c -> isAlphaNum c || c == '.') rest
                 in if null name then "Main" else name

-- | Convert a dotted module name to a relative file path.
-- E.g. @\"Examples.AssertCheckTest\"@ becomes @\"Examples\/AssertCheckTest.hs\"@.
moduleNameToPath :: String -> FilePath
moduleNameToPath modName = map dotToSlash modName ++ ".hs"
  where dotToSlash '.' = '/'
        dotToSlash c   = c

-- | Return the directory portion of a file path (everything up to the last @\/@).
-- Returns @\".\"@ for paths without a directory component.
parentDir :: FilePath -> FilePath
parentDir p = case reverse (dropWhile (/= '/') (reverse p)) of
    []  -> "."
    dir -> init dir  -- drop trailing slash

-- | The `summarizeResults` function evaluates the results of a test run
-- using the supplied `isSuccess` and `summarize_` functions from the adapters
summarizeResults :: (Summarizable s, TRun a s) =>
     a                                                            -- ^ The module to be evaluated
  -> [TestStr]                                                    -- ^ Tests we used to run analysis
  -> (Mutant, [InterpreterOutput s])                              -- ^ The mutant and its corresponding output of test runs.
  -> MutantSummary                                                -- ^ Returns a summary of the run for the mutant
summarizeResults m tests (mutant, ioresults) =
  case [e | Io (Left e) _ <- ioresults] of
    (I.WontCompile _ : _) -> MSumSkipped mutant logS
    (err:_)               -> MSumError mutant (showE err) logS
    []                    -> if any isKilled ioresults
                              then MSumKilled mutant logS
                              else MSumAlive mutant logS
  where isKilled (Io (Right x) _) = isFailure x
        isKilled _ = False
        logS :: [Summary]
        logS = zipWith (summarize mutant) tests ioresults
        summarize = summarize_ m

-- | Run all tests on a mutant
evalMutant ::
    (Typeable t, Summarizable t) =>
    -- | Optional timeout
    Maybe Int ->
    -- | Whether to delete the mutant file after evaluation
    Bool ->
    -- | Directory to write the mutant file into
    FilePath ->
    -- | Extra arguments forwarded to every test invocation
    [String] ->
    -- | The tests to be used
    [TestStr] ->
    -- | Mutant being tested
    Mutant ->
    -- | Returns the result of test runs
    IO [InterpreterOutput t]
evalMutant mtimeout doDelete mutantDir extraArgs tests Mutant{..} = do
    -- Write the mutant file to a path matching its module name so that GHC
    -- (via hint) can load it regardless of whether it enforces the
    -- file-path/module-name correspondence (behaviour that varies by GHC
    -- version).  A per-mutant hash subdirectory keeps concurrent mutants
    -- for the same module from colliding.
    let hashDir    = mutantDir ++ "/" ++ hash _mutant
        modRelPath = moduleNameToPath (extractModuleName _mutant)
        mutantFile = hashDir ++ "/" ++ modRelPath
        logF       = mutantFile ++ ".log"

    say mutantFile

    createDirectoryIfMissing True (parentDir mutantFile)
    writeResult <- try (writeFile mutantFile _mutant) :: IO (Either IOException ())
    result <- case writeResult of
        Left err -> return [Io{_io = Left (I.UnknownError ("write error: " ++ show err)), _ioLog = ""}]
        Right () -> stopFast (evalTest mtimeout extraArgs mutantFile logF) tests
    when doDelete $ do
        _ <- try (removeDirectoryRecursive hashDir) :: IO (Either IOException ())
        return ()
    return result

{- | Stop mutant runs at the first sign of problems (invalid mutants or test
failure).
-}
stopFast ::
    (Typeable t, Summarizable t) =>
    -- | The function that given a test, runs it, and returns the result
    (String -> IO (InterpreterOutput t)) ->
    -- | The tests to be run
    [TestStr] ->
    -- | Returns the output of all tests. If there is an error, then it will be at the last test.
    IO [InterpreterOutput t]
stopFast _ [] = return []
stopFast fn (x:xs) = do
  v <- fn x
  case _io v of
    Left r -> do  say (showE r)
                  return [v]
    Right out -> if isSuccess out
      then (v :) <$> stopFast fn xs
      else return [v] -- test failed (mutant detected)

-- | Show a clean, single-line error summary without raw exception traces.
showE :: I.InterpreterError -> String
showE (I.UnknownError e)    = "Error: " ++ firstLine e
showE (I.WontCompile [])    = "Compile error (no details)"
showE (I.WontCompile (e:_)) = "Compile error: " ++ firstLine (I.errMsg e)
showE (I.NotAllowed e)      = "Not allowed: " ++ firstLine e
showE (I.GhcException e)    = "GHC exception: " ++ firstLine e

-- | Return only the first line of a potentially multi-line string, truncated.
firstLine :: String -> String
firstLine s = take 200 $ takeWhile (/= '\n') s

-- | Run one single test on a mutant
evalTest ::
    (Typeable a, Summarizable a) =>
    -- | Optional timeout in microseconds
    Maybe Int ->
    -- | Extra arguments forwarded to the test via @withArgs@
    [String] ->
    -- | The mutant _file_ that we have to evaluate (_not_ the content)
    String ->
    -- | The file where we will write the stdout and stderr during the run.
    String ->
    -- | The test to be run
    TestStr ->
    -- | Returns the output of given test run
    IO (InterpreterOutput a)
evalTest mtimeout extraArgs mutantFile logF test = do
    -- On GHC 9.8+ the GHC API does not automatically read the
    -- .ghc.environment.* file written by `cabal build
    -- --write-ghc-environment-files=always`.  Detect the file in the current
    -- working directory and pass it explicitly via `-package-env` so that
    -- hint can find packages registered only in the local package database
    -- (e.g. the MuCheck library itself when the test suite runs it inline).
    pkgEnvArgs <- findPkgEnvArgs
    let runAction = withArgs extraArgs $ catchOutput logF $
                        IU.unsafeRunInterpreterWithArgs pkgEnvArgs (evalMethod mutantFile test)
    mval <- case mtimeout of
        Nothing -> Just <$> runAction
        Just t -> timeout t runAction
    let val = case mval of
            Nothing -> Left (I.UnknownError "Timeout occurred")
            Just v -> v
    return Io{_io = val, _ioLog = logF}

-- | Detect any @.ghc.environment.*@ file in the current directory and return
-- the corresponding @[\"-package-env\", \<file\>]@ arguments for
-- 'I.runInterpreterWithArgs'.  Returns @[]@ when no environment file is found
-- (e.g. in a plain @ghc@ or @runhaskell@ invocation without cabal).
findPkgEnvArgs :: IO [String]
findPkgEnvArgs = do
    cwd   <- getCurrentDirectory
    files <- listDirectory cwd
    let envFiles = filter (".ghc.environment." `isPrefixOf`) files
    case envFiles of
        (f:_) -> return ["-package-env", cwd ++ "/" ++ f]
        []    -> return []

{- | Given the filename, modulename, test to evaluate, evaluate, and return result as a pair.

> t = I.runInterpreter (evalMethod
>        "Examples/QuickCheckTest.hs"
>        "quickCheckResult idEmp")
-}
evalMethod ::
    (I.MonadInterpreter m, Typeable t) =>
    -- | The mutant _file_ to load
    String ->
    -- | The test to be run
    TestStr ->
    -- | Returns the monadic computation to be run by I.runInterpreter
    m t
evalMethod fileName evalStr = do
    I.loadModules [fileName]
    ms <- I.getLoadedModules
    I.setTopLevelModules ms
    I.interpret evalStr (I.as :: ((Typeable a) => IO a)) >>= liftIO

-- | Summarize the entire run. Passed results are per mutant
fullSummary :: (Show b, Summarizable b, TRun a b) =>
     a                                      -- ^ The module
  -> [TestStr]                              -- ^ The list of tests we used
  -> [[InterpreterOutput b]]                -- ^ The test ouput (per mutant, (per test))
  -> MAnalysisSummary                       -- ^ Returns the full summary of the run
fullSummary m _tests results = MAnalysisSummary {
  _maCoveredNumMutants = -1,
  _maNumMutants = length results,
  _maAlive = length alive,
  _maKilled = length fails,
  _maErrors = length runtimeErrors,
  _maSkipped = length skipErrors}
  where res = map (map _io) results
        -- A mutant is an error if any test resulted in an error
        (allErrors, completed) = partitionEithers $ map findError res
        findError r = case [e | Left e <- r] of
                        (e:_) -> Left e
                        []    -> Right [x | Right x <- r]
        -- Non-compilable mutants (WontCompile) are tracked separately as skipped
        (skipErrors, runtimeErrors) = partition isWontCompile allErrors
        isWontCompile (I.WontCompile _) = True
        isWontCompile _                 = False
        -- A mutant is killed if any test failed
        fails = filter (any (failure_ m)) completed
        -- A mutant is alive if all tests succeeded
        alive = filter (all (success_ m)) completed
