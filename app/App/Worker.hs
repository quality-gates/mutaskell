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

-- | Serialize a 'MutantSummary' to the line-based worker IPC format.
-- Does not serialize the 'Mutant' body; the parent already holds that.
workerSerialize :: MutantSummary -> String
workerSerialize ms =
  let (tag, logS, err) = case ms of
        MSumKilled  _ l   -> ("killed",  l, "")
        MSumAlive   _ l   -> ("alive",   l, "")
        MSumError   _ e l -> ("error",   l, e)
        MSumSkipped _ l   -> ("skipped", l, "")
        MSumOther   _ l   -> ("other",   l, "")
      logPaths = [p | Summary p <- logS]
  in unlines ([tag, err, show (length logPaths)] ++ logPaths)

-- | Deserialize a 'MutantSummary' from the worker IPC format.
-- Uses the supplied 'Mutant' (which the parent already holds) for the body.
workerDeserialize :: Mutant -> String -> MutantSummary
workerDeserialize mutant txt =
  case lines txt of
    (tag : err : logCountStr : rest) ->
      case reads logCountStr :: [(Int, String)] of
        [(n, "")] ->
          let logS = map Summary (take n rest)
          in case tag of
               "killed"  -> MSumKilled  mutant logS
               "alive"   -> MSumAlive   mutant logS
               "error"   -> MSumError   mutant err logS
               "skipped" -> MSumSkipped mutant logS
               _         -> MSumOther   mutant logS
        _ -> MSumError mutant "worker: parse error in result file" []
    _ -> MSumError mutant "worker: parse error in result file" []

-- | Remove flags that must not be forwarded to worker subprocesses.
filterWorkerArgs :: [String] -> [String]
filterWorkerArgs []                             = []
filterWorkerArgs ("--workers"      : _ : rest)  = filterWorkerArgs rest
filterWorkerArgs ("--run-mutant-id": _ : rest)  = filterWorkerArgs rest
filterWorkerArgs ("--worker-output": _ : rest)  = filterWorkerArgs rest
filterWorkerArgs (x                : rest)       = x : filterWorkerArgs rest
