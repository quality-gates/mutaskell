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
import Data.Ord (comparing, Down(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)

import Test.MuCheck (sampler)
import Test.MuCheck.AnalysisSummary (MAnalysisSummary(..))
import Test.MuCheck.Config (Config(..), defaultConfig, showMuVar)
import Test.MuCheck.Interpreter (MutantSummary(..), evalTest, evaluateMutants)
import Test.MuCheck.Mutation (genMutants, genMutantsFromAST, getASTFromStr, getAllTests)
import Test.MuCheck.TestAdapter (InterpreterOutput(..), Mutant(..), Summarizable(..), TRun(..))
import Test.MuCheck.TestAdapter.AssertCheckAdapter
import Test.MuCheck.Utils.Print


-- | Search for a .tix file in the current directory for --coverage auto-discovery.
findTixFile :: IO (Maybe FilePath)
findTixFile = do
  result <- try (listDirectory ".") :: IO (Either IOException [FilePath])
  case result of
    Left _   -> return Nothing
    Right fs -> return $ case filter (".tix" `isSuffixOf`) fs of
      (f:_) -> Just f
      []    -> Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-h" : _) -> help
    _          -> case parseOpts args of
      Left err      -> do putStrLn $ "Error: " ++ err; exitWith (ExitFailure 2)
      Right cliOpts -> do
        eConfigFn <- loadConfig (optConfig cliOpts)
        case eConfigFn of
          Left err -> do putStrLn $ "Config error: " ++ err; exitWith (ExitFailure 2)
          Right configFn -> do
            let baseOpts = configFn defaultOpts
            case parseOptsFrom baseOpts args of
              Left err   -> do putStrLn $ "Error: " ++ err; exitWith (ExitFailure 2)
              Right opts -> runOpts opts

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
    let pass = all (\r -> case _io r of { Right out -> isSuccess out; Left _ -> False }) results
    unless pass $ do
      putStrLn "Pre-flight check failed: test suite does not pass on unmodified source"
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
  case getASTFromStr src of
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

help :: IO ()
help = putStrLn $ showAS
  [ "Usage: mucheck [FLAGS] FILE"
  , ""
  , "FLAGS:"
  , "  -h                          Print this help"
  , "  --dry-run                   Show mutation counts by type without evaluating"
  , "  --noop                      Verify tests pass on unmodified source first (exit 3 on failure)"
  , "  --quiet                     Show only surviving mutants; suppress killed/error output"
  , "  --verbose                   Print full mutant source and test output during evaluation"
  , "  --debug                     Print stable IDs and raw interpreter diagnostics"
  , "  --no-diffs                  Suppress per-mutant unified diff output"
  , "  --fail-on-escaped           Exit with code 4 if any mutant survives"
  , "  --min-msi PCT               Exit with code 5 if MSI is below PCT percent"
  , "  --min-covered-msi PCT       Exit with code 5 if covered-code MSI is below PCT (requires -tix)"
  , "  --ignore-msi-with-no-mutations  Pass quality gates when no mutations are generated"
  , "  --disable NAME              Skip mutants of the named type (repeatable)"
  , "  --enable  NAME              Run only mutants of the named type (repeatable)"
  , "  --output-statuses CHARS     Show only result types matching chars: k=killed a=alive e=error s=skip"
  , "  -tix FILE                   HPC coverage file for coverage-guided mutation"
  , "  --coverage                  Auto-discover a .tix file in the current directory"
  , "  --timeout N                 Per-mutant timeout in seconds"
  , "  --timeout-coefficient N     Set timeout to N × measured baseline test-suite runtime"
  , "  --test-args ARG             Pass ARG to the test runner (repeatable)"
  , "  --config FILE               Load config from FILE instead of .mucheck.yaml"
  , "  --baseline FILE             Skip mutants whose ID appears in FILE from a previous run"
  , "  --update-baseline FILE      Write surviving mutant IDs to FILE after the run"
  , "  --blacklist FILE            Suppress mutations whose ID appears in FILE"
  , "  --run-mutant-id ID          Evaluate only the mutant with the given stable ID"
  , "  --git-diff-base REF         Skip mutation if file is not in 'git diff --name-only REF'"
  , "  --git-diff-lines            Restrict mutants to changed lines (requires --git-diff-base)"
  , "  --keep-mutants DIR          Write mutant files to DIR and keep them after evaluation"
  , "  --logger-json FILE          Write a compact JSON run summary to FILE"
  , "  --logger-html FILE          Write a standalone HTML mutation report to FILE"
  , "  --logger-github FILE        Write GitHub Actions annotations for escaped mutants to FILE"
  , "  --logger-gitlab FILE        Write GitLab Code Quality JSON for escaped mutants to FILE"
  , "  --logger-agentic-json FILE  Write per-mutant JSON for LLM consumption to FILE"
  , "  --workers N                 Number of parallel worker processes (default: 1)"
  , ""
  , "CONFIG FILE (.mucheck.yaml, auto-loaded from project root):"
  , "  min_msi: 80               Minimum required MSI (0-100)"
  , "  min_covered_msi: 80       Minimum required covered-code MSI"
  , "  timeout: 30               Per-mutant timeout in seconds"
  , "  quiet: true               Suppress killed/error output"
  , "  disable_mutators: [a, b]  Mutator names to skip"
  , "  enable_mutators: [a, b]   Restrict to named mutators"
  , "  exclude_dirs: [a, b]      Skip target if its path starts with any listed prefix"
  , "  workers: 4                Number of parallel worker processes"
  , ""
  , "MUTATOR NAMES (for --disable / --enable):"
  , "  pattern-match             Function pattern-match permutation and removal"
  , "  literal-values            Integer, float, char, string, and boolean literals"
  , "  functions                 Operator and function substitution"
  , "  negate-if-else            Swap if-then and if-else branches"
  , "  negate-guards             Wrap guard conditions in 'not'"
  , "  remove-not                Strip 'not' from negated sub-expressions"
  , "  remove-negation           Strip 'negate' and prefix '-' from expressions"
  , "  Trailing '*' is a prefix wildcard, e.g. 'other:*'"
  , ""
  , "EXIT CODES:"
  , "  0  Tests ran; no quality gate triggered"
  , "  2  Bad arguments"
  , "  3  Pre-flight failure (--noop: tests fail on original source)"
  , "  4  Escaped mutants (--fail-on-escaped)"
  , "  5  MSI below threshold (--min-msi / --min-covered-msi)"
  , ""
  , "E.g.:"
  , "  mucheck [--dry-run] [-tix file.tix] Examples/AssertCheckTest.hs"
  ]
