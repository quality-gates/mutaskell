{-# LANGUAGE OverloadedStrings #-}
-- | Subprocess-based parallel mutant evaluation.
-- hint is not thread-safe; each worker is a separate process.
module App.Worker
  ( runWithWorkers
  , evalOneWorker
  , workerSerialize
  , workerDeserialize
  , filterWorkerArgs
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import Control.Exception (IOException, try)
import Control.Monad (forM)
import Data.Aeson (encode, decode, object, (.=), withObject, (.:), (.:?))
import Data.Aeson.Types (parseMaybe, Parser)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.Process (createProcess, proc, waitForProcess)

import Test.MuCheck.AnalysisSummary (MAnalysisSummary)
import Test.MuCheck.Interpreter (MutantSummary(..), summaryFromMutantSummaries)
import Test.MuCheck.TestAdapter (Mutant(..), Summary(..))
import Test.MuCheck.Utils.Common (hash)

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
