{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{- | The Interpreter module is responible for invoking the Hint interpreter to
evaluate mutants.
-}
module Test.MuCheck.Interpreter (evaluateMutants, evalMethod, evalMutant, evalTest, summarizeResults, MutantSummary (..)) where

import Control.Exception (IOException, try)
import Control.Monad.Trans (liftIO)
import Data.Either (partitionEithers)
import Data.Typeable
import qualified Language.Haskell.Interpreter as I
import System.Directory (createDirectoryIfMissing)
import System.Environment (withArgs)
import System.Timeout (timeout)

import Test.MuCheck.AnalysisSummary
import Test.MuCheck.TestAdapter
import Test.MuCheck.Utils.Common
import Test.MuCheck.Utils.Print

-- | Data type to hold results of a single test execution
data MutantSummary = MSumError Mutant String [Summary]         -- ^ Capture the error if one occured
                   | MSumAlive Mutant [Summary]                -- ^ The mutant was alive
                   | MSumKilled Mutant [Summary]               -- ^ The mutant was kileld
                   | MSumOther Mutant [Summary]                -- ^ Undetermined - we will treat it as killed as it is not a success.
                   deriving (Show)

-- | Given the list of tests suites to check, run the test suite on mutants.
evaluateMutants ::
    (Show b, Summarizable b, TRun a b) =>
    -- | Optional timeout in microseconds
    Maybe Int ->
    -- | The module to be evaluated
    a ->
    -- | The mutants to be evaluated
    [Mutant] ->
    -- | The tests to be used for analysis
    [TestStr] ->
    -- | Returns a tuple of full run summary and individual mutant summary
    IO (MAnalysisSummary, [MutantSummary])
evaluateMutants mtimeout m mutants tests = do
    results <- mapM (evalMutant mtimeout tests) mutants -- [InterpreterOutput t]
    let singleTestSummaries = zipWith (curry (summarizeResults m tests)) mutants results
        ma = fullSummary m tests results
    return (ma, singleTestSummaries)

-- | The `summarizeResults` function evaluates the results of a test run
-- using the supplied `isSuccess` and `testSummaryFn` functions from the adapters
summarizeResults :: (Summarizable s, TRun a s) =>
     a                                                            -- ^ The module to be evaluated
  -> [TestStr]                                                    -- ^ Tests we used to run analysis
  -> (Mutant, [InterpreterOutput s])                              -- ^ The mutant and its corresponding output of test runs.
  -> MutantSummary                                                -- ^ Returns a summary of the run for the mutant
summarizeResults m tests (mutant, ioresults) = 
  case [e | Io (Left e) _ <- ioresults] of
    (err:_) -> MSumError mutant (show err) logS
    [] -> if any isKilled ioresults
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
    -- | The tests to be used
    [TestStr] ->
    -- | Mutant being tested
    Mutant ->
    -- | Returns the result of test runs
    IO [InterpreterOutput t]
evalMutant mtimeout tests Mutant{..} = do
    createDirectoryIfMissing True ".mutants"
    let mutantFile = ".mutants/" ++ hash _mutant ++ ".hs"

    say mutantFile

    writeResult <- try (writeFile mutantFile _mutant) :: IO (Either IOException ())
    case writeResult of
        Left err -> return [Io{_io = Left (I.UnknownError ("write error: " ++ show err)), _ioLog = ""}]
        Right () -> do
            let logF = mutantFile ++ ".log"
            stopFast (evalTest mtimeout mutantFile logF) tests

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

-- | Show error
showE :: I.InterpreterError -> String
showE (I.UnknownError e) = "Unknown: " ++ e
showE (I.WontCompile e) = "Compile: " ++ show e
showE (I.NotAllowed e) = "Not Allowed: " ++ e
showE (I.GhcException e) = "GhcException: " ++ e

-- | Run one single test on a mutant
evalTest ::
    (Typeable a, Summarizable a) =>
    -- | Optional timeout in microseconds
    Maybe Int ->
    -- | The mutant _file_ that we have to evaluate (_not_ the content)
    String ->
    -- | The file where we will write the stdout and stderr during the run.
    String ->
    -- | The test to be run
    TestStr ->
    -- | Returns the output of given test run
    IO (InterpreterOutput a)
evalTest mtimeout mutantFile logF test = do
    let runAction = withArgs [] $ catchOutput logF $ I.runInterpreter (evalMethod mutantFile test)
    mval <- case mtimeout of
        Nothing -> Just <$> runAction
        Just t -> timeout t runAction
    let val = case mval of
            Nothing -> Left (I.UnknownError "Timeout occurred")
            Just v -> v
    return Io{_io = val, _ioLog = logF}

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
  _maErrors= length errors}
  where res = map (map _io) results
        -- A mutant is an error if any test resulted in an error
        (errors, completed) = partitionEithers $ map findError res
        findError r = case [e | Left e <- r] of
                        (e:_) -> Left e
                        []    -> Right [x | Right x <- r]
        -- A mutant is killed if any test failed
        fails = filter (any (failure_ m)) completed
        -- A mutant is alive if all tests succeeded
        alive = filter (all (success_ m)) completed

