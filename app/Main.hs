module Main where

import App.Exit (applyExitPolicy, isSingleMutantMode)
import App.Filter
    ( applyAnnotations
    , applyBaselineCached
    , applyBlacklistCached
    , applyDiffLinesCached
    , applyDisableEnable
    , applyIgnoreLinesCached
    , applyRunMutantIdCached
    , cacheMutantIds
    , checkGitDiff
    , operatorSamplingEligible
    , parseAnnotations
    )
import App.Opts
import App.Orchestrator (runOrchestrator)
import App.Project (runProject, runProjectDryRun)
import App.Output
    ( prepareMutantDiffs
    , printMutatorBreakdown
    , printMutantDetailsWithDiffs
    , writeAgenticJsonLoggerWithDiffs
    , writeGithubLogger
    , writeGitlabLogger
    , writeHtmlLoggerWithDiffs
    , writeJsonLogger
    , writeUpdateBaseline
    )
import App.Worker (runWithWorkers, runWorkloadMode, workerSerialize, WorkloadBase(..))

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.List (group, isSuffixOf, isPrefixOf, sort, sortBy)
import Options.Applicative (execParser)
import Data.Ord (comparing, Down(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.IO (BufferMode (..), hFlush, hPutStr, hPutStrLn, hSetBuffering, stderr, stdout)

import Test.Mutaskell (sampler)
import Test.Mutaskell.AnalysisSummary (MAnalysisSummary(..))
import Test.Mutaskell.Config (Config(..), defaultConfig, showMuVar)
import Test.Mutaskell.Interpreter (MutantSummary(..), evalTest, evaluateMutants)
import Test.Mutaskell.Mutation
    ( genMutants, genMutantsFromAST, genSampledMutants, getASTFromFile
    , getASTFromStr, getAllTests )
import Test.Mutaskell.TestAdapter (InterpreterOutput(..), Mutant(..), Summarizable(..), TRun(..))
import Test.Mutaskell.TestAdapter.AssertCheckAdapter


-- | Search for a .tix file in the current directory for --coverage auto-discovery.
findTixFile :: IO (Maybe FilePath)
findTixFile = do
  result <- try (listDirectory ".") :: IO (Either IOException [FilePath])
  case result of
    Left _   -> return Nothing
    Right fs -> return $ case filter (".tix" `isSuffixOf`) fs of
      (f:_) -> Just f
      []    -> Nothing

-- | Scan args for a --config value without a full parse.
extractConfigArg :: [String] -> Maybe FilePath
extractConfigArg ("--config" : v : _) = Just v
extractConfigArg (_ : rest)            = extractConfigArg rest
extractConfigArg []                    = Nothing

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let configPath = extractConfigArg args
  eConfigFn <- loadConfig configPath
  case eConfigFn of
    Left err -> do putStrLn $ "Config error: " ++ err; exitWith (ExitFailure 2)
    Right configFn -> do
      let baseOpts = configFn defaultOpts
      opts <- execParser (optsParserInfo baseOpts)
      case validateOpts opts of
        Left err        -> do putStrLn $ "Error: " ++ err; exitWith (ExitFailure 2)
        Right validOpts -> runOpts validOpts

-- | Dispatch on the target: a directory enters project mode (walk the whole
-- repo, drive its real build/test); a file uses the per-file path below.
runOpts :: Opts -> IO ()
runOpts opts = do
  isDir <- doesDirectoryExist (optFile opts)
  if isDir
    then if optDryRun opts then runProjectDryRun opts else runProject opts
    else runOptsFile opts

-- | Trace one candidate-generation invocation to stderr when MUCHECK_TRACE is
-- set.  The number of these lines per run is the generation count the
-- benchmark reports.  A worker child evaluates the workload it is handed and
-- emits none, so a child that started regenerating candidates would show up
-- as extra trace lines.
traceGeneration :: String -> IO ()
traceGeneration what = do
  tracing <- lookupEnv "MUCHECK_TRACE"
  when (isJust tracing) $
    hPutStrLn stderr ("trace: generation invocation (" ++ what ++ ")")

runOptsFile :: Opts -> IO ()
runOptsFile opts
  -- Worker child mode: the parent hands us a workload document holding the
  -- mutant it already selected, so we evaluate it directly.  No candidate
  -- generation, test discovery or timeout calibration happens here.
  | Just workloadPath <- optRunMutantWorkload opts = do
      runWorkloadMode workloadPath (optWorkerOutput opts)
      exitSuccess
  | optDryRun opts = dryRun (optFile opts)
  | optExec opts   = runOrchestrator opts
  | otherwise      = do
      let file = optFile opts
          excDirs = optExcludeDirs opts
          inExcluded = any (\d -> d `isPrefixOf` file || (d ++ "/") `isPrefixOf` file) excDirs
      when inExcluded $ do
        putStrLn $ "Skipping " ++ file ++ ": excluded by exclude_dirs"
        exitSuccess
      inDiff <- checkGitDiff file (optGitDiffBase opts)
      unless inDiff $ do
        putStrLn $ "Skipping " ++ file ++ ": not in git diff relative to " ++ fromMaybe "" (optGitDiffBase opts)
        return ()
      when inDiff $ do
        when (optNoop opts) $ noopCheck (optFile opts)
        origSrc <- readFile (optFile opts)
        let modFile  = toRun (optFile opts) :: AssertCheckRun
            anns     = parseAnnotations origSrc
            maxN     = fromMaybe (maxNumMutants defaultConfig) (optMaxMutants opts)
        res <-
          if operatorSamplingEligible opts anns
            then
              -- No coverage requirement or deterministic filter applies, so
              -- the sample quota can be spent before any mutant is applied
              -- and rendered.  The covered count is unknown in the same way
              -- as a run without coverage data.
              let cfg = defaultConfig { maxNumMutants = maxN }
              in do
                eAst <- getASTFromStr origSrc
                case eAst of
                  Left err  -> return (Left err)
                  Right ast -> Right . (,) (-1) <$> genSampledMutants cfg ast
            else do
              tix <- if optCoverage opts && null (optTix opts)
                     then do
                       mf <- findTixFile
                       case mf of
                         Just f  -> hPutStrLn stderr ("Coverage: using " ++ f) >> return f
                         Nothing
                           | isJust (optMinCoveredMsi opts) -> do
                               hPutStrLn stderr ("Coverage: no .tix file found; " ++ missingCoverageForMinCoveredMsi)
                               exitWith (ExitFailure 2)
                           | otherwise -> hPutStrLn stderr "Coverage: no .tix file found; proceeding without" >> return ""
                     else return (optTix opts)
              genMutants (getName modFile) tix
        (len, mutants) <- case res of
          Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 2)
          Right r -> return r
        traceGeneration "parent"
        -- Apply all deterministic filters before sampling so the sample quota is
        -- spent only on candidates that survive every filter.
        let filtered0 = applyDisableEnable (optDisable opts) (optEnable opts) mutants
            filtered1 = applyAnnotations anns filtered0
            cached1   = cacheMutantIds filtered1
        cached2 <- applyBaselineCached  (optBaseline opts)  cached1
        cached3 <- applyBlacklistCached (optBlacklist opts) cached2
        cached4 <- applyDiffLinesCached (optFile opts) (optGitDiffBase opts) (optGitDiffLines opts) cached3
        let cached5   = applyIgnoreLinesCached origSrc (optIgnoreLines opts) cached4
            preFilter = map fst (applyRunMutantIdCached (optRunMutantId opts) cached5)
        finalMutants <- sampler (defaultConfig { maxNumMutants = maxN }) preFilter
        let tests = map (genTest modFile)
        testRes <- getAllTests (getName modFile)
        testNames <- case testRes of
          Left err -> hPutStrLn stderr ("Parse error: " ++ err) >> exitWith (ExitFailure 2)
          Right names -> return names
        when (optSkipWithoutTest opts && null testNames) $ do
          putStrLn $ "Skipping " ++ optFile opts ++ ": no test annotations found"
          exitSuccess
        timeoutUs <- resolveTimeout opts (optFile opts) modFile testNames
        let total = length finalMutants
        progressRef <- newIORef (0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int)
        let progressCallback ms = modifyIORef' progressRef $ \(k,a,e,sk) -> case ms of
              MSumKilled  _ _   -> (k+1, a,   e,   sk)
              MSumAlive   _ _   -> (k,   a+1, e,   sk)
              MSumError   {}    -> (k,   a,   e+1, sk)
              MSumSkipped _ _   -> (k,   a,   e,   sk+1)
              MSumOther   _ _   -> (k+1, a,   e,   sk)
            suppressProgress = optQuiet opts || optSilent opts || workerMode
            workerMode = isJust (optWorkerOutput opts)
            mcallback = if suppressProgress || total == 0 then Nothing else Just progressCallback
        let progressLoop = do
              (k,a,e,sk) <- readIORef progressRef
              let done = k + a + e + sk
              hPutStr stderr $ "\rProgress: [" ++ show done ++ "/" ++ show total ++ "]"
                ++ " killed=" ++ show k ++ " alive=" ++ show a
                ++ " error=" ++ show e ++ " skip=" ++ show sk ++ "   "
              hFlush stderr
              threadDelay 200000
              progressLoop
        mtid <- if suppressProgress || total == 0
          then return Nothing
          else fmap Just (forkIO progressLoop)
        (fsum', tsum) <-
          if optWorkers opts > 1
            then do
              -- Workers evaluate the mutants the parent already generated;
              -- the shared settings travel with each mutant as a workload.
              let wbase = WorkloadBase
                    { wbTarget      = file
                    , wbTests       = tests testNames
                    , wbTimeout     = timeoutUs
                    , wbKeepMutants = optKeepMutants opts
                    , wbTestArgs    = optTestArgs opts
                    }
              runWithWorkers (optWorkers opts) wbase finalMutants progressCallback
            else evaluateMutants 1 timeoutUs (optKeepMutants opts) (optTestArgs opts) mcallback modFile finalMutants (tests testNames)
        case mtid of
          Nothing  -> return ()
          Just tid -> do
            killThread tid
            (k,a,e,sk) <- readIORef progressRef
            let done = k + a + e + sk
            hPutStr stderr $ "\rProgress: [" ++ show done ++ "/" ++ show total ++ "]"
              ++ " killed=" ++ show k ++ " alive=" ++ show a
              ++ " error=" ++ show e ++ " skip=" ++ show sk ++ "   "
            hPutStrLn stderr ""
            hFlush stderr
        case optWorkerOutput opts of
          Just outFile -> do
            case tsum of
              (ms : _) -> BL.writeFile outFile (workerSerialize ms)
              []       -> return ()
            exitSuccess
          Nothing -> return ()
        let msum = case len of
                     -1 -> fsum' { _maCoveredNumMutants = -1 }
                     _  -> fsum' { _maCoveredNumMutants = length mutants }
            reportDiffs = prepareMutantDiffs origSrc tsum
        unless (optSilent opts) $ printMutantDetailsWithDiffs opts reportDiffs
        unless (isSingleMutantMode opts) $ do
          print msum
          unless (optSilent opts) $ printMutatorBreakdown opts tsum
          writeJsonLogger opts msum
          writeGithubLogger opts (optFile opts) tsum
          writeGitlabLogger opts (optFile opts) tsum
          writeAgenticJsonLoggerWithDiffs opts (optFile opts) origSrc reportDiffs msum
          writeHtmlLoggerWithDiffs opts (optFile opts) origSrc reportDiffs msum
          writeUpdateBaseline opts tsum
          applyExitPolicy opts msum

noopCheck :: FilePath -> IO ()
noopCheck file = do
  testRes <- getAllTests file
  tests <- case testRes of
    Left err -> hPutStrLn stderr ("Parse error: " ++ err) >> exitWith (ExitFailure 2)
    Right t -> return t
  unless (null tests) $ do
    let testStrs = map (genTest (toRun file :: AssertCheckRun)) tests
        logF     = ".mucheck-noop.log"
        runTest :: String -> IO (InterpreterOutput AssertCheckSummary)
        runTest  = evalTest Nothing [] file logF
    results <- mapM runTest testStrs
    let firstError = foldr (\r acc -> case _io r of
                                Left e  -> Just e
                                Right _ -> acc) Nothing results
        pass = all (\r -> case _io r of { Right out -> isSuccess out; Left _ -> False }) results
    unless pass $ do
      putStrLn "Pre-flight check failed: test suite does not pass on unmodified source"
      case firstError of
        Just e  -> hPutStrLn stderr $ "  Interpreter error: " ++ show e
        Nothing -> return ()
      exitWith (ExitFailure 3)

-- | Resolve the per-mutant timeout in microseconds.
-- If --timeout-coefficient is set, measure baseline runtime and scale it.
-- If --timeout is set, use it directly. If neither, return Nothing.
resolveTimeout :: Opts -> FilePath -> AssertCheckRun -> [String] -> IO (Maybe Int)
resolveTimeout opts file modFile testNames =
  case optTimeoutCoef opts of
    Nothing  -> return $ fmap (* 1000000) (optTimeout opts)
    Just coef -> do
      let testStrs = map (genTest modFile) testNames
          logF = ".mucheck-baseline-timing.log"
          runOne :: String -> IO (InterpreterOutput AssertCheckSummary)
          runOne = evalTest Nothing [] file logF
      t0 <- getCurrentTime
      mapM_ runOne testStrs
      t1 <- getCurrentTime
      let baselineSeconds = realToFrac (diffUTCTime t1 t0) :: Double
          timeoutUs = round (coef * baselineSeconds * 1e6) :: Int
      return $ Just (max 1000000 timeoutUs)

dryRun :: FilePath -> IO ()
dryRun file = do
  result <- getASTFromFile file
  case result of
    Left err -> hPutStrLn stderr ("Parse error: " ++ err) >> exitWith (ExitFailure 2)
    Right ast -> do
      let mutants = genMutantsFromAST defaultConfig ast
      traceGeneration "dry-run"
      let byType  = [(v, length g) | g@(v:_) <- group . sort $ map _mtype mutants]
          byType' = sortBy (comparing (Down . snd)) byType
          -- 7 is seeded into the list so 'maximum' never sees [] (a zero-mutant
          -- file, e.g. a pure re-export module, used to crash here).
          colW    = maximum (7 : map (length . showMuVar . fst) byType')
          pad s   = s ++ replicate (colW - length s + 2) ' '
          sep     = replicate (colW + 10) '-'
          rows    = map (\(v, n) -> "  " ++ pad (showMuVar v) ++ show n) byType'
          total   = length mutants
      putStrLn $ "  " ++ pad "Mutator" ++ "Count"
      putStrLn sep
      mapM_ putStrLn rows
      putStrLn sep
      putStrLn $ "  " ++ pad "Total" ++ show total
      putStrLn "(upper bound; identical mutations are deduplicated before evaluation)"
