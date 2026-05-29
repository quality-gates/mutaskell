{-# LANGUAGE RecordWildCards #-}
module Main where

import App.Exit (applyExitPolicy, isSingleMutantMode)
import App.Filter
    ( applyAnnotations
    , applyBaseline
    , applyBlacklist
    , applyDiffLines
    , applyDisableEnable
    , applyIgnoreLines
    , applyRunMutantId
    , checkGitDiff
    , parseAnnotations
    )
import App.Opts
import App.Output
    ( printMutantDetails
    , printMutatorBreakdown
    , writeAgenticJsonLogger
    , writeGithubLogger
    , writeGitlabLogger
    , writeHtmlLogger
    , writeJsonLogger
    , writeUpdateBaseline
    )
import App.Worker (runWithWorkers, workerSerialize)

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.List (group, isSuffixOf, isPrefixOf, sort, sortBy)
import Options.Applicative (execParser)
import Data.Ord (comparing, Down(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)

import Test.Muskell (sampler)
import Test.Muskell.AnalysisSummary (MAnalysisSummary(..))
import Test.Muskell.Config (Config(..), defaultConfig, showMuVar)
import Test.Muskell.Interpreter (MutantSummary(..), evalTest, evaluateMutants)
import Test.Muskell.Mutation (genMutants, genMutantsFromAST, getASTFromStr, getAllTests)
import Test.Muskell.TestAdapter (InterpreterOutput(..), Mutant(..), Summarizable(..), TRun(..))
import Test.Muskell.TestAdapter.AssertCheckAdapter


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

runOpts :: Opts -> IO ()
runOpts opts
  | optDryRun opts = dryRun (optFile opts)
  | otherwise      = do
      let file = optFile opts
          excDirs = optExcludeDirs opts
          inExcluded = any (\d -> d `isPrefixOf` file || (d ++ "/") `isPrefixOf` file) excDirs
      when inExcluded $ do
        putStrLn $ "Skipping " ++ file ++ ": excluded by exclude_dirs"
        exitWith ExitSuccess
      inDiff <- checkGitDiff file (optGitDiffBase opts)
      unless inDiff $ do
        putStrLn $ "Skipping " ++ file ++ ": not in git diff relative to " ++ maybe "" id (optGitDiffBase opts)
        return ()
      when inDiff $ do
        when (optNoop opts) $ noopCheck (optFile opts)
        origSrc <- readFile (optFile opts)
        let modFile  = toRun (optFile opts) :: AssertCheckRun
            anns     = parseAnnotations origSrc
        tix <- if optCoverage opts && null (optTix opts)
               then do
                 mf <- findTixFile
                 case mf of
                   Just f  -> hPutStrLn stderr ("Coverage: using " ++ f) >> return f
                   Nothing -> hPutStrLn stderr "Coverage: no .tix file found; proceeding without" >> return ""
               else return (optTix opts)
        res <- genMutants (getName modFile) tix
        (len, mutants) <- case res of
          Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 2)
          Right r -> return r
        -- Apply all deterministic filters before sampling so the sample quota is
        -- spent only on candidates that survive every filter.
        let filtered0 = applyDisableEnable (optDisable opts) (optEnable opts) mutants
            filtered1 = applyAnnotations anns filtered0
        filtered2 <- applyBaseline  (optBaseline opts)  filtered1
        filtered3 <- applyBlacklist (optBlacklist opts) filtered2
        filtered4 <- applyDiffLines (optFile opts) (optGitDiffBase opts) (optGitDiffLines opts) filtered3
        let filtered5 = applyIgnoreLines origSrc (optIgnoreLines opts) filtered4
            preFilter = applyRunMutantId (optRunMutantId opts) filtered5
            maxN      = fromMaybe (maxNumMutants defaultConfig) (optMaxMutants opts)
        finalMutants <- sampler (defaultConfig { maxNumMutants = maxN }) preFilter
        let tests = map (genTest modFile)
        testRes <- getAllTests (getName modFile)
        testNames <- case testRes of
          Left err -> hPutStrLn stderr ("Parse error: " ++ err) >> exitWith (ExitFailure 2)
          Right names -> return names
        when (optSkipWithoutTest opts && null testNames) $ do
          putStrLn $ "Skipping " ++ optFile opts ++ ": no test annotations found"
          exitWith ExitSuccess
        timeoutUs <- resolveTimeout opts (optFile opts) modFile testNames
        let total = length finalMutants
        progressRef <- newIORef (0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int)
        let progressCallback ms = modifyIORef' progressRef $ \(k,a,e,sk) -> case ms of
              MSumKilled  _ _   -> (k+1, a,   e,   sk)
              MSumAlive   _ _   -> (k,   a+1, e,   sk)
              MSumError   _ _ _ -> (k,   a,   e+1, sk)
              MSumSkipped _ _   -> (k,   a,   e,   sk+1)
              MSumOther   _ _   -> (k+1, a,   e,   sk)
            suppressProgress = optQuiet opts || optSilent opts || workerMode
            workerMode = optWorkerOutput opts /= Nothing
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
              origArgs <- getArgs
              runWithWorkers (optWorkers opts) origArgs finalMutants progressCallback
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
              (ms : _) -> writeFile outFile (workerSerialize ms)
              []       -> return ()
            exitWith ExitSuccess
          Nothing -> return ()
        let msum = case len of
                     -1 -> fsum' { _maCoveredNumMutants = -1 }
                     _  -> fsum' { _maCoveredNumMutants = length mutants }
        unless (optSilent opts) $ printMutantDetails opts origSrc tsum
        unless (isSingleMutantMode opts) $ do
          print msum
          unless (optSilent opts) $ printMutatorBreakdown opts tsum
          writeJsonLogger opts msum
          writeGithubLogger opts (optFile opts) tsum
          writeGitlabLogger opts (optFile opts) tsum
          writeAgenticJsonLogger opts (optFile opts) origSrc tsum msum
          writeHtmlLogger opts (optFile opts) origSrc tsum msum
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
      _ <- mapM runOne testStrs
      t1 <- getCurrentTime
      let baselineSeconds = realToFrac (diffUTCTime t1 t0) :: Double
          timeoutUs = round (coef * baselineSeconds * 1e6) :: Int
      return $ Just (max 1000000 timeoutUs)

dryRun :: FilePath -> IO ()
dryRun file = do
  src <- readFile file
  result <- getASTFromStr src
  case result of
    Left err -> hPutStrLn stderr ("Parse error: " ++ err) >> exitWith (ExitFailure 2)
    Right ast -> do
      let mutants = genMutantsFromAST defaultConfig ast
          byType  = [(v, length g) | g@(v:_) <- group . sort $ map _mtype mutants]
          byType' = sortBy (comparing (Down . snd)) byType
          colW    = max 7 $ maximum $ map (length . showMuVar . fst) byType'
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

