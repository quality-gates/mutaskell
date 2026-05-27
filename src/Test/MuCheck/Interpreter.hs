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
data MutantSummary
    = -- | Capture the error if one occured
      MSumError Mutant String [Summary]
    | -- | The mutant was alive
      MSumAlive Mutant [Summary]
    | -- | The mutant was kileld
      MSumKilled Mutant [Summary]
    | -- | Undetermined - we will treat it as killed as it is not a success.
      MSumOther Mutant [Summary]
    deriving (Show, Typeable)

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

{- | The `summarizeResults` function evaluates the results of a test run
using the supplied `isSuccess` and `testSummaryFn` functions from the adapters
-}
summarizeResults ::
    (Summarizable s, TRun a s) =>
    -- | The module to be evaluated
    a ->
    -- | Tests we used to run analysis
    [TestStr] ->
    -- | The mutant and its corresponding output of test runs.
    (Mutant, [InterpreterOutput s]) ->
    -- | Returns a summary of the run for the mutant
    MutantSummary
summarizeResults m tests (mutant, ioresults) = case last results of -- the last result should indicate status because we dont run if there is error.
    Left err -> MSumError mutant (show err) logS
    Right out -> myresult out
  where
    results = map _io ioresults
    myresult out
        | isSuccess out = MSumAlive mutant logS
        | isFailure out = MSumKilled mutant logS
        | otherwise = MSumOther mutant logS
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
stopFast fn (x : xs) = do
    v <- fn x
    case _io v of
        Left r -> do
            say (showE r)
            -- do not append results of the run because mutant was non viable unless it was the last
            if null xs
                then return [v]
                else stopFast fn xs
        Right out ->
            if isSuccess out
                then (v :) <$> stopFast fn xs
                else return [v] -- test failed (mutant detected)

-- | Show error
showE :: I.InterpreterError -> String
showE (I.UnknownError e) = "Unknown: " ++ e
showE (I.WontCompile e) = "Compile: " ++ show (head e)
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
fullSummary ::
    (Show b, Summarizable b, TRun a b) =>
    -- | The module
    a ->
    -- | The list of tests we used
    [TestStr] ->
    -- | The test ouput (per mutant, (per test))
    [[InterpreterOutput b]] ->
    -- | Returns the full summary of the run
    MAnalysisSummary
fullSummary m _tests results =
    MAnalysisSummary
        { _maCoveredNumMutants = -1
        , _maNumMutants = length results
        , _maAlive = length alive
        , _maKilled = length fails
        , _maErrors = length errors
        }
  where
    res = map (map _io) results
    lasts = map last res -- get the last test runs
    (errors, completed) = partitionEithers lasts
    fails = filter (failure_ m) completed -- look if others failed or not
    alive = filter (success_ m) completed
