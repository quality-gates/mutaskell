{-# LANGUAGE OverloadedStrings #-}
-- | Subprocess-based parallel mutant evaluation.
-- hint is not thread-safe; each worker is a separate process.
module App.Worker
  ( runWithWorkers
  , evalOneWorker
  , workerSerialize
  , workerDeserialize
  , filterWorkerArgs
  , Workload(..)
  , WorkloadBase(..)
  , mutantWorkload
  , encodeWorkload
  , decodeWorkload
  , decodeWorkloadFile
  , evalWorkload
  , runWorkloadMode
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import Control.Exception (IOException, try)
import Control.Monad (forM, when)
import Data.Aeson (encode, decode, eitherDecode, object, (.=), withObject, (.:), (.:?), Value)
import Data.Aeson.Types (parseEither, parseMaybe, Parser, (.!=))
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.Process (createProcess, proc, waitForProcess)
import Trace.Hpc.Util (fromHpcPos)

import Test.Mutaskell.AnalysisSummary (MAnalysisSummary)
import Test.Mutaskell.Config (MuVar(..), parseMuVar, showMuVar)
import Test.Mutaskell.Interpreter (MutantSummary(..), evaluateMutants, summaryFromMutantSummaries)
import Test.Mutaskell.TestAdapter (Mutant(..), Summary(..), toRun)
import Test.Mutaskell.TestAdapter.AssertCheckAdapter (AssertCheckRun(..))
import Test.Mutaskell.Tix (Span, toSpan)
import Test.Mutaskell.Utils.Common (hash)

-- | Run mutant evaluation using N parallel worker subprocesses.
-- Each worker is a fresh mucheck process that evaluates a single mutant via
-- @--run-mutant-id@ and writes its 'MutantSummary' to a temp file.
-- hint is not thread-safe; process-level isolation provides safety.
runWithWorkers :: Int -> [String] -> [Mutant] -> (MutantSummary -> IO ()) -> IO (MAnalysisSummary, [MutantSummary])
runWithWorkers numWorkers origArgs mutants callback = do
  exe    <- getExecutablePath
  tmpDir <- getTemporaryDirectory
  let baseArgs = filterWorkerArgs origArgs
  sem    <- newQSem numWorkers
  resultVars <- forM mutants $ \mutant -> do
    var <- newEmptyMVar
    _ <- forkIO $ do
      waitQSem sem
      result <- evalOneWorker exe tmpDir baseArgs mutant
      callback result
      putMVar var result
      signalQSem sem
    return var
  summaries <- mapM takeMVar resultVars
  return (summaryFromMutantSummaries summaries, summaries)

-- | Evaluate a single mutant by spawning a fresh mucheck subprocess.
evalOneWorker :: FilePath -> FilePath -> [String] -> Mutant -> IO MutantSummary
evalOneWorker exe tmpDir baseArgs mutant = do
  let mid        = hash (_mutant mutant)
      resultFile = tmpDir ++ "/mucheck-worker-" ++ mid ++ ".txt"
      childArgs  = ["--run-mutant-id", mid, "--worker-output", resultFile] ++ baseArgs
  (_, _, _, ph) <- createProcess (proc exe childArgs)
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> do
      eContent <- try $ do
        str <- readFile resultFile
        let n = length str
        n `seq` return str
      _ <- try (removeFile resultFile) :: IO (Either IOException ())
      case eContent of
        Left  ioerr ->
          return $ MSumError mutant ("worker: read error: " ++ show (ioerr :: IOException)) []
        Right content -> return $ workerDeserialize mutant content
    ExitFailure code ->
      return $ MSumError mutant ("worker: subprocess exited with code " ++ show code) []

-- | Serialize a 'MutantSummary' to a self-contained JSON object.
-- A single extra newline inside a diff or test output cannot corrupt the
-- deserialiser since JSON handles embedded newlines safely.
-- Does not serialize the 'Mutant' body; the parent already holds that.
workerSerialize :: MutantSummary -> String
workerSerialize ms = BL.unpack $ encode $ object
    [ "version"   .= (1 :: Int)
    , "result"    .= tag
    , "error"     .= err
    , "summaries" .= logPaths
    ]
  where
    (tag, err, logS) = case ms of
      MSumKilled  _ l   -> ("killed"  :: String, "" :: String, l)
      MSumAlive   _ l   -> ("alive",   "",  l)
      MSumError   _ e l -> ("error",   e,   l)
      MSumSkipped _ l   -> ("skipped", "",  l)
      MSumOther   _ l   -> ("other",   "",  l)
    logPaths = [p | Summary p <- logS]

-- | Deserialize a 'MutantSummary' from the JSON worker IPC format.
-- Uses the supplied 'Mutant' (which the parent already holds) for the body.
workerDeserialize :: Mutant -> String -> MutantSummary
workerDeserialize mutant txt =
  case decode (BL.pack txt) >>= parseMaybe parseResult of
    Nothing -> MSumError mutant "worker: JSON parse/schema error in result file" []
    Just ms -> ms
  where
    parseResult = withObject "WorkerResult" $ \o -> do
      _ <- (o .:? "version" :: Parser (Maybe Int))
      result    <- o .: "result"
      err       <- o .: "error"
      summaries <- o .: "summaries"
      let logS = map Summary summaries
      return $ case (result :: String) of
        "killed"  -> MSumKilled  mutant logS
        "alive"   -> MSumAlive   mutant logS
        "error"   -> MSumError   mutant err logS
        "skipped" -> MSumSkipped mutant logS
        _         -> MSumOther   mutant logS

-- | Remove flags that must not be forwarded to worker subprocesses.
filterWorkerArgs :: [String] -> [String]
filterWorkerArgs []                             = []
filterWorkerArgs ("--workers"      : _ : rest)  = filterWorkerArgs rest
filterWorkerArgs ("--run-mutant-id": _ : rest)  = filterWorkerArgs rest
filterWorkerArgs ("--worker-output": _ : rest)  = filterWorkerArgs rest
filterWorkerArgs (x                : rest)       = x : filterWorkerArgs rest

-- | Version of the workload transport format. Bumped on incompatible changes
-- so an old child rejects a document it cannot interpret.
workloadVersion :: Int
workloadVersion = 1

-- | The evaluation settings every worker child shares. The parent resolves
-- them once; each child receives them inside its per-mutant 'Workload'.
data WorkloadBase = WorkloadBase
  { wbTarget      :: FilePath     -- ^ original source file the mutants came from
  , wbTests       :: [String]     -- ^ fully-built test strings to run
  , wbTimeout     :: Maybe Int    -- ^ per-mutant timeout in microseconds
  , wbKeepMutants :: Maybe FilePath -- ^ keep-dir for mutant files (Nothing = temp, deleted)
  , wbTestArgs    :: [String]     -- ^ extra args forwarded to test invocations
  } deriving (Eq, Show)

-- | The complete input for one worker child: the parent's already-selected
-- mutant plus the effective evaluation settings. Carrying the mutant itself
-- is what lets the child skip candidate generation entirely.
data Workload = Workload
  { wTarget      :: FilePath     -- ^ original source file the mutant came from
  , wMutant      :: Mutant       -- ^ the exact mutant the parent selected
  , wTests       :: [String]     -- ^ fully-built test strings to run
  , wTimeout     :: Maybe Int    -- ^ per-mutant timeout in microseconds
  , wKeepMutants :: Maybe FilePath
  , wTestArgs    :: [String]
  } deriving (Eq, Show)

-- | Build the per-mutant workload a child evaluates from the shared
-- settings and the parent's selected mutant.
mutantWorkload :: WorkloadBase -> Mutant -> Workload
mutantWorkload base' m = Workload
  { wTarget      = wbTarget base'
  , wMutant      = m
  , wTests       = wbTests base'
  , wTimeout     = wbTimeout base'
  , wKeepMutants = wbKeepMutants base'
  , wTestArgs    = wbTestArgs base'
  }

-- | Serialize a 'Workload' to the JSON transport format.
encodeWorkload :: Workload -> String
encodeWorkload w = BL.unpack $ encode $ object
    [ "version"    .= workloadVersion
    , "target"     .= wTarget w
    , "mutant"     .= _mutant m
    , "mutator"    .= showMuVar (_mtype m)
    , "span"       .= spanCoords (_mspan m)
    , "tests"      .= wTests w
    , "timeout"    .= wTimeout w
    , "keepMutants".= wKeepMutants w
    , "testArgs"   .= wTestArgs w
    ]
  where m = wMutant w

-- | The four span coordinates, as written to and read back from JSON.
spanCoords :: Span -> [Int]
spanCoords sp = let (l1, c1, l2, c2) = fromHpcPos sp in [l1, c1, l2, c2]

-- | Deserialize a 'Workload' from the JSON transport format.
-- Malformed or missing data is reported as a classified error carrying the
-- @worker:@ prefix used for all child-side failures.
decodeWorkload :: String -> Either String Workload
decodeWorkload txt = do
    val <- either (\e -> Left ("worker: workload JSON parse error: " ++ e)) Right
             (eitherDecode (BL.pack txt) :: Either String Value)
    case parseEither parseWorkload val of
        Left err -> Left ("worker: workload schema error: " ++ err)
        Right wl -> Right wl
  where
    parseWorkload = withObject "Workload" $ \o -> do
        ver <- o .: "version" :: Parser Int
        when (ver /= workloadVersion) $
            fail ("unsupported transport version " ++ show ver)
        msrc    <- o .: "mutant"
        mutName <- o .: "mutator"
        mtype   <- case parseMuVar mutName of
            Just mv -> pure mv
            Nothing -> fail ("unknown mutator name: " ++ mutName)
        coords  <- o .: "span" :: Parser [Int]
        sp      <- case coords of
            [l1, c1, l2, c2] -> pure (toSpan (l1, c1, l2, c2))
            _                -> fail ("expected 4 span coordinates, got " ++ show coords)
        target  <- o .: "target"
        tests   <- o .: "tests"
        mtimeout    <- o .:? "timeout"
        mKeepMutants<- o .:? "keepMutants"
        testArgs    <- o .:? "testArgs" .!= []
        pure $ Workload
            { wTarget      = target
            , wMutant      = Mutant{_mutant = msrc, _mtype = mtype, _mspan = sp}
            , wTests       = tests
            , wTimeout     = mtimeout
            , wKeepMutants = mKeepMutants
            , wTestArgs    = testArgs
            }

-- | Read and decode a workload file. A missing or unreadable file is a
-- classified error, matching the child's other failure modes.
decodeWorkloadFile :: FilePath -> IO (Either String Workload)
decodeWorkloadFile path = do
    result <- try (readFile path >>= \s -> length s `seq` return s)
                :: IO (Either IOException String)
    return $ case result of
        Left err -> Left ("worker: cannot read workload: " ++ show err)
        Right s  -> decodeWorkload s

-- | Placeholder mutant for results produced before a workload could be read.
-- The parent re-attaches the real mutant when deserialising the result.
workloadErrorMutant :: Mutant
workloadErrorMutant = Mutant
    { _mutant = ""
    , _mtype  = MutateOther "workload"
    , _mspan  = toSpan (0, 0, 0, 0)
    }

-- | Evaluate a transported workload in this process and classify the result.
--
-- This is the child-side evaluation. The mutant arrives fully formed, so no
-- candidate generation runs here: the child writes the mutant file, runs the
-- transported tests against it, and summarises, exactly as the serial path
-- does for a single mutant.
evalWorkload :: Workload -> IO MutantSummary
evalWorkload wl = do
    let modFile = toRun (wTarget wl) :: AssertCheckRun
    (_, summaries) <- evaluateMutants
        1                       -- serial within the child; parallelism is the parent's job
        (wTimeout wl)
        (wKeepMutants wl)
        (wTestArgs wl)
        Nothing
        modFile
        [wMutant wl]
        (wTests wl)
    return $ case summaries of
        (s:_) -> s
        []    -> MSumError (wMutant wl) "worker: evaluation produced no summary" []

-- | Child entry point: evaluate the workload document at the given path and
-- write the result JSON to the worker-output file, if one was requested.
runWorkloadMode :: FilePath -> Maybe FilePath -> IO ()
runWorkloadMode workloadPath mOut = do
    eWl <- decodeWorkloadFile workloadPath
    summary <- case eWl of
        Left err -> return $ MSumError workloadErrorMutant err []
        Right wl -> evalWorkload wl
    case mOut of
        Nothing -> return ()
        Just f  -> writeFile f (workerSerialize summary)
