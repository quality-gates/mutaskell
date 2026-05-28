{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (unless, when, forM_)
import Data.Char (isSpace)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (group, intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, sort, sortBy, stripPrefix)
import Data.Ord (comparing, Down(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)
import System.Process (readProcess)

import Test.MuCheck (sampler)
import Test.MuCheck.AnalysisSummary (MAnalysisSummary(..))
import Test.MuCheck.Config (MuVar(..), defaultConfig)
import Test.MuCheck.Interpreter (MutantSummary(..), evalTest, evaluateMutants)
import Test.MuCheck.Mutation (genMutants, genMutantsFromAST, getASTFromStr, getAllTests)
import Test.MuCheck.TestAdapter (InterpreterOutput(..), Mutant(..), Summarizable(..), TRun(..))
import Test.MuCheck.TestAdapter.AssertCheckAdapter
import Test.MuCheck.Tix (spanStartLine)
import Test.MuCheck.Utils.Print
import Test.MuCheck.Utils.Common (hash)


data Opts = Opts
  { optFile         :: FilePath
  , optTix          :: FilePath
  , optDryRun       :: Bool
  , optNoop         :: Bool
  , optFailOnEscape :: Bool
  , optMinMsi       :: Maybe Int
  , optMinCoveredMsi :: Maybe Int
  , optDisable      :: [String]
  , optEnable       :: [String]
  , optConfig       :: Maybe FilePath
  , optQuiet        :: Bool
  , optVerbose      :: Bool
  , optDebug        :: Bool
  , optNoDiffs      :: Bool
  , optIgnoreMsiNoMutations :: Bool
  , optOutputStatuses :: Maybe String
  , optTimeout      :: Maybe Int
  , optLoggerJson   :: Maybe FilePath
  , optBaseline     :: Maybe FilePath
  , optUpdateBaseline :: Maybe FilePath
  , optBlacklist    :: Maybe FilePath
  , optRunMutantId  :: Maybe String
  , optLoggerGithub :: Maybe FilePath
  , optLoggerGitlab :: Maybe FilePath
  , optTimeoutCoef  :: Maybe Double
  , optGitDiffBase  :: Maybe String
  , optGitDiffLines :: Bool
  , optKeepMutants  :: Maybe FilePath
  , optLoggerAgenticJson :: Maybe FilePath
  , optLoggerHtml   :: Maybe FilePath
  , optTestArgs     :: [String]
  , optCoverage     :: Bool
  , optSilent       :: Bool
  , optMaxMutants   :: Maybe Int
  , optIgnoreLines  :: [String]
  , optSkipWithoutTest :: Bool
  }

defaultOpts :: Opts
defaultOpts = Opts
  { optFile         = ""
  , optTix          = ""
  , optDryRun       = False
  , optNoop         = False
  , optFailOnEscape = False
  , optMinMsi       = Nothing
  , optMinCoveredMsi = Nothing
  , optDisable      = []
  , optEnable       = []
  , optConfig       = Nothing
  , optQuiet        = False
  , optVerbose      = False
  , optDebug        = False
  , optNoDiffs      = False
  , optIgnoreMsiNoMutations = False
  , optOutputStatuses = Nothing
  , optTimeout      = Nothing
  , optLoggerJson   = Nothing
  , optBaseline     = Nothing
  , optUpdateBaseline = Nothing
  , optBlacklist    = Nothing
  , optRunMutantId  = Nothing
  , optLoggerGithub = Nothing
  , optLoggerGitlab = Nothing
  , optTimeoutCoef  = Nothing
  , optGitDiffBase  = Nothing
  , optGitDiffLines = False
  , optKeepMutants  = Nothing
  , optLoggerAgenticJson = Nothing
  , optLoggerHtml   = Nothing
  , optTestArgs     = []
  , optCoverage     = False
  , optSilent       = False
  , optMaxMutants   = Nothing
  , optIgnoreLines  = []
  , optSkipWithoutTest = False
  }

knownConfigKeys :: [String]
knownConfigKeys =
  [ "min_msi", "min_covered_msi", "timeout", "quiet", "silent_mode"
  , "max_mutants", "json_output", "html_output"
  , "disable_mutators", "enable_mutators"
  , "ignore_source_lines", "skip_without_test"
  ]

-- | Load config and return either an error string or a transformer.
-- Applied to defaultOpts before CLI parsing so CLI flags override config.
loadConfig :: Maybe FilePath -> IO (Either String (Opts -> Opts))
loadConfig mPath = do
  let path = case mPath of { Just p -> p; Nothing -> ".mucheck.yaml" }
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left _        -> return (Right id)
    Right contents ->
      let pairs = parseYamlKV contents
          unknowns = filter (\(k,_) -> k `notElem` knownConfigKeys) pairs
      in case unknowns of
           ((k,_):_) -> return (Left $ "Unknown config key: " ++ k
                                  ++ ". Known keys: " ++ intercalate ", " knownConfigKeys)
           []        -> return (Right (applyYamlConfig pairs))

parseYamlKV :: String -> [(String, String)]
parseYamlKV = concatMap parseLine . filter (not . skip) . lines
  where
    skip s = case dropWhile isSpace s of { [] -> True; (c:_) -> c == '#' }
    parseLine s = case break (== ':') s of
      (k, ':' : v) ->
        let k' = trim k; v' = dropWhile isSpace v
        in if null k' then [] else [(k', v')]
      _ -> []
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

applyYamlConfig :: [(String, String)] -> Opts -> Opts
applyYamlConfig pairs opts = foldl applyPair opts pairs
  where
    applyPair o ("min_msi", v) = case (reads v, optMinMsi o) of
      ([(i,"")], Nothing) -> o { optMinMsi = Just i }
      _                   -> o
    applyPair o ("min_covered_msi", v) = case (reads v, optMinCoveredMsi o) of
      ([(i,"")], Nothing) -> o { optMinCoveredMsi = Just i }
      _                   -> o
    applyPair o ("timeout", v) = case (reads v, optTimeout o) of
      ([(i,"")], Nothing) -> o { optTimeout = Just i }
      _                   -> o
    applyPair o ("max_mutants", v) = case (reads v, optMaxMutants o) of
      ([(i,"")], Nothing) -> o { optMaxMutants = Just i }
      _                   -> o
    applyPair o ("quiet", v)
      | v `elem` ["true","True","yes"] = o { optQuiet = True }
      | otherwise                       = o
    applyPair o ("silent_mode", v)
      | v `elem` ["true","True","yes"] = o { optSilent = True }
      | otherwise                       = o
    applyPair o ("json_output", v) = case optLoggerJson o of
      Nothing -> o { optLoggerJson = Just (trim v) }
      Just _  -> o
    applyPair o ("html_output", v) = case optLoggerHtml o of
      Nothing -> o { optLoggerHtml = Just (trim v) }
      Just _  -> o
    applyPair o ("disable_mutators", v) =
      o { optDisable = optDisable o ++ parseYamlList v }
    applyPair o ("enable_mutators", v) =
      o { optEnable = optEnable o ++ parseYamlList v }
    applyPair o ("ignore_source_lines", v) =
      o { optIgnoreLines = optIgnoreLines o ++ parseYamlList v }
    applyPair o ("skip_without_test", v)
      | v `elem` ["true","True","yes"] = o { optSkipWithoutTest = True }
      | otherwise                       = o
    applyPair o _ = o
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseYamlList :: String -> [String]
parseYamlList s = case dropWhile isSpace s of
  ('[' : rest) ->
    let inner = takeWhile (/= ']') rest
    in map trim (splitOn ',' inner)
  s' -> let t = trim s' in if null t then [] else [t]
  where trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Search for a .tix file in the current directory for --coverage auto-discovery.
findTixFile :: IO (Maybe FilePath)
findTixFile = do
  result <- try (listDirectory ".") :: IO (Either IOException [FilePath])
  case result of
    Left _   -> return Nothing
    Right fs -> return $ case filter (".tix" `isSuffixOf`) fs of
      (f:_) -> Just f
      []    -> Nothing

parseOpts :: [String] -> Either String Opts
parseOpts = parseOptsFrom defaultOpts

parseOptsFrom :: Opts -> [String] -> Either String Opts
parseOptsFrom base = go base
  where
    go opts ("--dry-run"         : rest) = go opts { optDryRun       = True } rest
    go opts ("--noop"            : rest) = go opts { optNoop         = True } rest
    go opts ("--fail-on-escaped" : rest) = go opts { optFailOnEscape = True } rest
    go _    ("--min-msi"         : [])   = Left "--min-msi requires an integer argument"
    go opts ("--min-msi" : n     : rest) =
      case reads n of
        [(i, "")] -> go opts { optMinMsi = Just i } rest
        _         -> Left $ "--min-msi requires an integer argument, got: " ++ n
    go _    ("--min-covered-msi" : [])   = Left "--min-covered-msi requires an integer argument"
    go opts ("--min-covered-msi" : n : rest) =
      case reads n of
        [(i, "")] -> go opts { optMinCoveredMsi = Just i } rest
        _         -> Left $ "--min-covered-msi requires an integer argument, got: " ++ n
    go _    ("-tix"              : [])   = Left "-tix requires a file path argument"
    go opts ("-tix" : tix        : rest) = go opts { optTix = tix } rest
    go _    ("--disable"         : [])   = Left "--disable requires a name argument"
    go _    ("--disable" : "*"   : _)    = Left "--disable: bare '*' not allowed; use a prefix like 'functions/*'"
    go opts ("--disable" : n     : rest) = go opts { optDisable = n : optDisable opts } rest
    go _    ("--enable"          : [])   = Left "--enable requires a name argument"
    go _    ("--enable"  : "*"   : _)    = Left "--enable: bare '*' not allowed; use a prefix like 'functions/*'"
    go opts ("--enable"  : n     : rest) = go opts { optEnable = n : optEnable opts } rest
    go _    ("--config"          : [])   = Left "--config requires a file path argument"
    go opts ("--config"  : file  : rest) = go opts { optConfig = Just file } rest
    go opts ("--quiet"           : rest) = go opts { optQuiet = True } rest
    go opts ("--verbose"         : rest) = go opts { optVerbose = True } rest
    go opts ("--debug"           : rest) = go opts { optDebug = True } rest
    go opts ("--no-diffs"        : rest) = go opts { optNoDiffs = True } rest
    go opts ("--ignore-msi-with-no-mutations" : rest) = go opts { optIgnoreMsiNoMutations = True } rest
    go _    ("--output-statuses" : [])   = Left "--output-statuses requires a string argument"
    go opts ("--output-statuses" : chars : rest) = go opts { optOutputStatuses = Just chars } rest
    go _    ("--timeout"         : [])   = Left "--timeout requires an integer argument"
    go opts ("--timeout" : n     : rest) =
      case reads n of
        [(i, "")] -> go opts { optTimeout = Just i } rest
        _         -> Left $ "--timeout requires an integer argument, got: " ++ n
    go _    ("--logger-json"     : [])   = Left "--logger-json requires a file path argument"
    go opts ("--logger-json" : f : rest) = go opts { optLoggerJson = Just f } rest
    go _    ("--baseline"        : [])   = Left "--baseline requires a file path argument"
    go opts ("--baseline" : f    : rest) = go opts { optBaseline = Just f } rest
    go _    ("--update-baseline" : [])   = Left "--update-baseline requires a file path argument"
    go opts ("--update-baseline" : f : rest) = go opts { optUpdateBaseline = Just f } rest
    go _    ("--blacklist"       : [])   = Left "--blacklist requires a file path argument"
    go opts ("--blacklist" : f   : rest) = go opts { optBlacklist = Just f } rest
    go _    ("--run-mutant-id"   : [])   = Left "--run-mutant-id requires an ID argument"
    go opts ("--run-mutant-id" : i : rest) = go opts { optRunMutantId = Just i } rest
    go _    ("--logger-github"   : [])   = Left "--logger-github requires a file path argument"
    go opts ("--logger-github" : f : rest) = go opts { optLoggerGithub = Just f } rest
    go _    ("--logger-gitlab"   : [])   = Left "--logger-gitlab requires a file path argument"
    go opts ("--logger-gitlab" : f : rest) = go opts { optLoggerGitlab = Just f } rest
    go _    ("--timeout-coefficient" : []) = Left "--timeout-coefficient requires a number argument"
    go opts ("--timeout-coefficient" : n : rest) =
      case reads n of
        [(d, "")] -> go opts { optTimeoutCoef = Just d } rest
        _         -> Left $ "--timeout-coefficient requires a number, got: " ++ n
    go _    ("--git-diff-base"   : [])   = Left "--git-diff-base requires a ref argument"
    go opts ("--git-diff-base" : r : rest) = go opts { optGitDiffBase = Just r } rest
    go opts ("--git-diff-lines"  : rest) = go opts { optGitDiffLines = True } rest
    go _    ("--keep-mutants"    : [])   = Left "--keep-mutants requires a directory argument"
    go opts ("--keep-mutants" : d : rest) = go opts { optKeepMutants = Just d } rest
    go _    ("--logger-agentic-json" : []) = Left "--logger-agentic-json requires a file path argument"
    go opts ("--logger-agentic-json" : f : rest) = go opts { optLoggerAgenticJson = Just f } rest
    go _    ("--logger-html"    : [])   = Left "--logger-html requires a file path argument"
    go opts ("--logger-html" : f : rest) = go opts { optLoggerHtml = Just f } rest
    go _    ("--test-args"      : [])   = Left "--test-args requires an argument"
    go opts ("--test-args" : a  : rest) = go opts { optTestArgs = optTestArgs opts ++ [a] } rest
    go opts ("--coverage"       : rest) = go opts { optCoverage = True } rest
    go _    (arg@('-' : _)       : _)    = Left $ "Unknown flag: " ++ arg
    go opts (file                : _)    = Right opts { optFile = file }
    go _    []                           = Left "Need a file argument"

-- | Match a user-supplied pattern against a mutator name.
-- Trailing '*' acts as a prefix wildcard: "other:*" matches "other:remove-not".
matchesPat :: String -> String -> Bool
matchesPat pat name = case reverse pat of
  ('*' : revPrefix) -> reverse revPrefix `isPrefixOf` name
  _                 -> pat == name

-- | Apply --enable / --disable filters to a list of sampled mutants.
applyDisableEnable :: [String] -> [String] -> [Mutant] -> [Mutant]
applyDisableEnable disable enable ms
  | not (null enable)  = filter (\m -> any (\p -> matchesPat p (muName m)) enable)  ms
  | not (null disable) = filter (\m -> not $ any (\p -> matchesPat p (muName m)) disable) ms
  | otherwise          = ms
  where muName = showMuVar . _mtype

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
      inDiff <- checkGitDiff (optFile opts) (optGitDiffBase opts)
      unless inDiff $ do
        putStrLn $ "Skipping " ++ optFile opts ++ ": not in git diff relative to " ++ maybe "" id (optGitDiffBase opts)
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
        (len, mutants) <- genMutants (getName modFile) tix
        smutants        <- sampler defaultConfig mutants
        let capMutants ms = case optMaxMutants opts of
              Just n  -> take n ms
              Nothing -> ms
            filtered0 = applyDisableEnable (optDisable opts) (optEnable opts) (capMutants smutants)
            filtered1 = applyAnnotations anns filtered0
        filtered2 <- applyBaseline  (optBaseline opts)  filtered1
        filtered3 <- applyBlacklist (optBlacklist opts) filtered2
        filtered4 <- applyDiffLines (optFile opts) (optGitDiffBase opts) (optGitDiffLines opts) filtered3
        let filtered5 = applyIgnoreLines origSrc (optIgnoreLines opts) filtered4
            finalMutants = applyRunMutantId (optRunMutantId opts) filtered5
            tests        = map (genTest modFile)
        testNames <- getAllTests (getName modFile)
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
            suppressProgress = optQuiet opts || optSilent opts
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
        (fsum', tsum) <- evaluateMutants timeoutUs (optKeepMutants opts) (optTestArgs opts) mcallback modFile finalMutants (tests testNames)
        case mtid of
          Nothing  -> return ()
          Just tid -> do
            killThread tid
            -- Read final counts in the main thread (avoids output ordering race).
            -- The background thread is dead; this is safe to read without a lock.
            (k,a,e,sk) <- readIORef progressRef
            let done = k + a + e + sk
            hPutStr stderr $ "\rProgress: [" ++ show done ++ "/" ++ show total ++ "]"
              ++ " killed=" ++ show k ++ " alive=" ++ show a
              ++ " error=" ++ show e ++ " skip=" ++ show sk ++ "   "
            hPutStrLn stderr ""
            hFlush stderr
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

-- | True when --run-mutant-id is set (single-mutant mode skips aggregate output).
isSingleMutantMode :: Opts -> Bool
isSingleMutantMode = maybe False (const True) . optRunMutantId

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

-- | Human-readable description of what a mutator changes.
mutatorDescription :: MuVar -> String
mutatorDescription MutatePatternMatch              = "Permute or remove a function pattern match"
mutatorDescription MutateValues                    = "Replace a literal value with a neighbouring value"
mutatorDescription MutateFunctions                 = "Replace an operator or function with a similar one"
mutatorDescription MutateNegateIfElse              = "Swap the then and else branches of an if expression"
mutatorDescription MutateNegateGuards              = "Wrap a guard condition in 'not'"
mutatorDescription (MutateOther "remove-not")      = "Remove 'not' from a negated sub-expression"
mutatorDescription (MutateOther "remove-negation") = "Remove 'negate' or prefix '-' from an expression"
mutatorDescription (MutateOther "case-alt-remove") = "Remove one alternative from a case expression"
mutatorDescription (MutateOther "case-default-remove") = "Remove the catch-all alternative from a case or guard"
mutatorDescription (MutateOther "remove-stmt")     = "Remove one statement from a do-block"
mutatorDescription (MutateOther "remove-let-binding") = "Remove one binding from a let or where clause"
mutatorDescription (MutateOther "remove-where-binding") = "Remove one binding from a where clause"
mutatorDescription (MutateOther "remove-self-assign") = "Remove a self-assignment (let x = x or x <- return x)"
mutatorDescription (MutateOther "negate-literal")  = "Replace a positive numeric literal with its negation"
mutatorDescription (MutateOther "string-literal")  = "Replace a string literal in a comparison with \"\""
mutatorDescription (MutateOther "bool-operand")    = "Replace a Boolean operand in && or || with True or False"
mutatorDescription (MutateOther "flip-maybe")      = "Flip Just x to Nothing or Nothing to Just undefined"
mutatorDescription (MutateOther "flip-either")     = "Flip Right x to Left x or Left x to Right x"
mutatorDescription (MutateOther "remove-forkIO")   = "Remove forkIO/async/withAsync concurrency wrapper"
mutatorDescription (MutateOther "bracket-degenerate") = "Replace bracket with acquire >>= action, removing cleanup"
mutatorDescription (MutateOther "error-guard")     = "Replace exception handler with a no-op"
mutatorDescription (MutateOther "replace-mutable-arg") = "Replace IORef/MVar/TVar argument with undefined"
mutatorDescription (MutateOther "zero-return")     = "Replace function body with zero value for declared return type"
mutatorDescription (MutateOther s)                 = "Apply mutator: " ++ s

-- | Write a per-mutant agentic JSON file for LLM consumption.
writeAgenticJsonLogger :: Opts -> FilePath -> String -> [MutantSummary] -> MAnalysisSummary -> IO ()
writeAgenticJsonLogger opts file origSrc tsum msum = case optLoggerAgenticJson opts of
  Nothing   -> return ()
  Just path -> do
    let MAnalysisSummary{..} = msum
        noerrors = _maNumMutants - _maErrors
        msiVal :: Double
        msiVal = if noerrors > 0
                 then fromIntegral _maKilled / fromIntegral noerrors
                 else 0.0
        resultOf (MSumKilled  _ _)   = "killed"  :: String
        resultOf (MSumAlive   _ _)   = "alive"
        resultOf (MSumError   _ _ _) = "error"
        resultOf (MSumSkipped _ _)   = "skipped"
        resultOf (MSumOther   _ _)   = "other"
        mutOf (MSumKilled  m _)   = m
        mutOf (MSumAlive   m _)   = m
        mutOf (MSumError   m _ _) = m
        mutOf (MSumSkipped m _)   = m
        mutOf (MSumOther   m _)   = m
        oLines = lines origSrc
        contextWindow = 3
        contextFor ln =
          let start = max 1 (ln - contextWindow)
              end   = min (length oLines) (ln + contextWindow)
              numbered = zip [start..] (drop (start - 1) (take end oLines))
          in  concatMap (\(i, l) -> "    " ++ show i ++ ": " ++ l ++ "\\n") numbered
        entry s =
          let m   = mutOf s
              ln  = spanStartLine (_mspan m)
              res = resultOf s
              d   = show $ unifiedDiff origSrc (_mutant m)
              desc = mutatorDescription (_mtype m)
              mid  = hash (_mutant m)
              ctx  = contextFor ln
          in  intercalate "\n"
              [ "  {"
              , "    \"id\": " ++ show mid ++ ","
              , "    \"type\": " ++ show (showMuVar (_mtype m)) ++ ","
              , "    \"file\": " ++ show file ++ ","
              , "    \"line\": " ++ show ln ++ ","
              , "    \"description\": " ++ show desc ++ ","
              , "    \"context_start_line\": " ++ show (max 1 (ln - contextWindow)) ++ ","
              , "    \"context\": " ++ show ctx ++ ","
              , "    \"diff\": " ++ d ++ ","
              , "    \"result\": " ++ show res ++ ","
              , "    \"reminder\": \"If result is alive, this mutation was not detected by any test. Consider adding a test that exercises this code path.\""
              , "  }"
              ]
        entries = map entry tsum
        mutantsBody = intercalate ",\n" entries
        summaryJson = intercalate "\n"
          [ "  \"summary\": {"
          , "    \"total\": " ++ show _maNumMutants ++ ","
          , "    \"killed\": " ++ show _maKilled ++ ","
          , "    \"alive\": " ++ show _maAlive ++ ","
          , "    \"skipped\": " ++ show _maSkipped ++ ","
          , "    \"errors\": " ++ show _maErrors ++ ","
          , "    \"msi\": " ++ show msiVal
          , "  }"
          ]
        json = "{\n  \"mutants\": [\n" ++ mutantsBody ++
               (if null entries then "" else "\n") ++
               "  ],\n" ++ summaryJson ++ "\n}\n"
    writeFile path json

-- | Write a standalone HTML mutation report.
writeHtmlLogger :: Opts -> FilePath -> String -> [MutantSummary] -> MAnalysisSummary -> IO ()
writeHtmlLogger opts file origSrc tsum msum = case optLoggerHtml opts of
  Nothing   -> return ()
  Just path -> writeFile path (buildHtmlReport file origSrc tsum msum)

buildHtmlReport :: FilePath -> String -> [MutantSummary] -> MAnalysisSummary -> String
buildHtmlReport file origSrc tsum msum =
  let MAnalysisSummary{..} = msum
      noerrors = _maNumMutants - _maErrors
      msiPct :: Int
      msiPct = if noerrors > 0 then _maKilled * 100 `div` noerrors else 0
      esc = concatMap escChar
      escChar '<' = "&lt;"; escChar '>' = "&gt;"
      escChar '&' = "&amp;"; escChar '"' = "&quot;"
      escChar c   = [c]
      statusClass (MSumKilled  _ _)   = "killed"  :: String
      statusClass (MSumAlive   _ _)   = "alive"
      statusClass (MSumError   _ _ _) = "error"
      statusClass (MSumSkipped _ _)   = "skipped"
      statusClass (MSumOther   _ _)   = "other"
      statusLabel (MSumKilled  _ _)   = "KILLED"  :: String
      statusLabel (MSumAlive   _ _)   = "ALIVE"
      statusLabel (MSumError   _ _ _) = "ERROR"
      statusLabel (MSumSkipped _ _)   = "SKIPPED"
      statusLabel (MSumOther   _ _)   = "OTHER"
      mutOf (MSumKilled  m _)   = m; mutOf (MSumAlive   m _)   = m
      mutOf (MSumError   m _ _) = m; mutOf (MSumSkipped m _)   = m
      mutOf (MSumOther   m _)   = m
      oLines = lines origSrc
      ctxWin = 3 :: Int
      contextRows m =
        let sl    = spanStartLine (_mspan m)
            start = max 1 (sl - ctxWin)
            end   = min (length oLines) (sl + ctxWin)
            nums  = [start..end]
            lns   = drop (start - 1) (take end oLines)
        in concatMap (\(i,l) ->
              "<tr" ++ (if i == sl then " class=\"hl\"" else "") ++ ">"
              ++ "<td class=\"ln\">" ++ show i ++ "</td>"
              ++ "<td><code>" ++ esc l ++ "</code></td></tr>")
           (zip nums lns)
      diffBlock m =
        let d = unifiedDiff origSrc (_mutant m)
        in if null d then "<em>no diff</em>"
           else "<pre class=\"diff\">" ++ esc d ++ "</pre>"
      entryHtml s =
        let m  = mutOf s
            sc = statusClass s
            sl = statusLabel s
            mid = hash (_mutant m)
        in "<div class=\"mutant " ++ sc ++ "\">"
           ++ "<div class=\"mh\"><span class=\"st " ++ sc ++ "\">" ++ sl ++ "</span>"
           ++ " <span class=\"mv\">" ++ esc (showMuVar (_mtype m)) ++ "</span>"
           ++ " <span class=\"id\">ID:" ++ esc mid ++ "</span></div>"
           ++ "<div class=\"ctx\"><table>" ++ contextRows m ++ "</table></div>"
           ++ "<div class=\"df\">" ++ diffBlock m ++ "</div></div>\n"
      css = concat
        [ "body{font-family:monospace;margin:20px}"
        , ".sum{background:#f5f5f5;padding:12px;border-radius:4px;margin-bottom:16px}"
        , ".mutant{border:1px solid #ddd;margin-bottom:12px;border-radius:4px;overflow:hidden}"
        , ".mh{padding:6px 10px;display:flex;gap:12px;align-items:center}"
        , ".killed .mh{background:#d4edda}.alive .mh{background:#f8d7da}"
        , ".error .mh{background:#fff3cd}.skipped .mh{background:#e2e3e5}"
        , ".st{padding:2px 6px;border-radius:3px;font-size:.85em;font-weight:bold}"
        , ".st.killed{background:#28a745;color:#fff}.st.alive{background:#dc3545;color:#fff}"
        , ".st.error{background:#ffc107;color:#000}.st.skipped{background:#6c757d;color:#fff}"
        , ".id{color:#666;font-size:.8em}.mv{font-weight:bold}"
        , ".ctx table{border-collapse:collapse;width:100%;padding:4px 8px}"
        , ".ctx td{padding:1px 6px;font-size:.85em}.ln{color:#999;text-align:right;border-right:1px solid #eee}"
        , ".hl td{background:#fffde7}"
        , ".df pre{margin:0;padding:6px 10px;background:#f8f8f8;font-size:.85em;overflow-x:auto}"
        ]
  in "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">"
     ++ "<title>MuCheck: " ++ esc file ++ "</title>"
     ++ "<style>" ++ css ++ "</style></head><body>"
     ++ "<h1>Mutation Report: " ++ esc file ++ "</h1>"
     ++ "<div class=\"sum\"><strong>MSI:</strong> " ++ show msiPct ++ "% &nbsp; "
     ++ "<strong>Total:</strong> " ++ show _maNumMutants ++ " &nbsp; "
     ++ "<strong>Killed:</strong> " ++ show _maKilled ++ " &nbsp; "
     ++ "<strong>Alive:</strong> " ++ show _maAlive ++ " &nbsp; "
     ++ "<strong>Skipped:</strong> " ++ show _maSkipped ++ " &nbsp; "
     ++ "<strong>Errors:</strong> " ++ show _maErrors ++ "</div>"
     ++ concatMap entryHtml tsum
     ++ "</body></html>\n"

-- | Filter out mutants whose source start line contains any of the given substrings.
applyIgnoreLines :: String -> [String] -> [Mutant] -> [Mutant]
applyIgnoreLines _   []       ms = ms
applyIgnoreLines src patterns ms = filter (not . isIgnored) ms
  where
    srcLines = lines src
    isIgnored m =
      let sl = spanStartLine (_mspan m)
          ln = if sl >= 1 && sl <= length srcLines then srcLines !! (sl - 1) else ""
      in  any (`isInfixOf` ln) patterns

-- | Return True if --git-diff-base is not set, or if the file appears in the diff.
checkGitDiff :: FilePath -> Maybe String -> IO Bool
checkGitDiff _ Nothing = return True
checkGitDiff file (Just ref) = do
  result <- try (readProcess "git" ["diff", "--name-only", ref] "") :: IO (Either IOException String)
  case result of
    Left _       -> return True  -- git not available; proceed normally
    Right output ->
      let changed = lines output
      in  return $ any (\c -> file == c || isSuffixOf c file || isSuffixOf file c) changed

-- | If --git-diff-lines is active (requires --git-diff-base), filter mutants
-- to those whose start line falls within lines changed relative to the base ref.
applyDiffLines :: FilePath -> Maybe String -> Bool -> [Mutant] -> IO [Mutant]
applyDiffLines _    Nothing  _     ms = return ms
applyDiffLines _    _        False ms = return ms
applyDiffLines file (Just ref) True ms = do
  result <- try (readProcess "git" ["diff", "--unified=0", ref, "--", file] "") :: IO (Either IOException String)
  case result of
    Left _       -> return ms
    Right output ->
      let changedLines = parseDiffChangedLines output
          inChanged m  = spanStartLine (_mspan m) `elem` changedLines
      in  return $ filter inChanged ms

-- | Parse `git diff --unified=0` output and return all changed line numbers in the new file.
parseDiffChangedLines :: String -> [Int]
parseDiffChangedLines = concatMap parseHunk . lines
  where
    parseHunk line = case stripPrefix "@@ " line of
      Nothing   -> []
      Just rest ->
        let plusPart = dropWhile (/= '+') rest
        in  case stripPrefix "+" plusPart of
              Nothing -> []
              Just s  ->
                let (startStr, afterStart) = break (\c -> c == ',' || c == ' ') s
                in  case reads startStr of
                      [(start, "")] ->
                        let count = case afterStart of
                                      (',' : cs) -> case reads (takeWhile (/= ' ') cs) of
                                                      [(n, "")] -> n
                                                      _         -> 1
                                      _          -> 1
                        in  [start .. start + count - 1]
                      _ -> []

-- | Write GitHub Actions annotation lines for escaped mutants.
writeGithubLogger :: Opts -> FilePath -> [MutantSummary] -> IO ()
writeGithubLogger opts file tsum = case optLoggerGithub opts of
  Nothing   -> return ()
  Just path -> do
    let aliveSums = [s | s@(MSumAlive _ _) <- tsum]
        line s = case s of
          MSumAlive m _ ->
            let ln  = spanStartLine (_mspan m)
                msg = "Mutant survived: " ++ showMuVar (_mtype m) ++ " (ID: " ++ hash (_mutant m) ++ ")"
            in "::warning file=" ++ file ++ ",line=" ++ show ln ++ ",col=1::" ++ msg
          _ -> ""
        annotations = map line aliveSums
    writeFile path (unlines annotations)

-- | Write a GitLab Code Quality JSON artifact for escaped mutants.
writeGitlabLogger :: Opts -> FilePath -> [MutantSummary] -> IO ()
writeGitlabLogger opts file tsum = case optLoggerGitlab opts of
  Nothing   -> return ()
  Just path -> do
    let aliveMs = [m | MSumAlive m _ <- tsum]
        entry m =
          let ln   = spanStartLine (_mspan m)
              fp   = hash (_mutant m)
              desc = "Mutant survived: " ++ showMuVar (_mtype m)
          in intercalate "\n"
             [ "  {"
             , "    \"description\": " ++ show desc ++ ","
             , "    \"fingerprint\": " ++ show fp ++ ","
             , "    \"severity\": \"major\","
             , "    \"location\": {"
             , "      \"path\": " ++ show file ++ ","
             , "      \"lines\": { \"begin\": " ++ show ln ++ " }"
             , "    }"
             , "  }"
             ]
        entries = map entry aliveMs
        body    = intercalate ",\n" entries
    writeFile path $ "[\n" ++ body ++ (if null entries then "" else "\n") ++ "]\n"

-- | Keep only the mutant matching the given stable ID; return all if Nothing.
applyRunMutantId :: Maybe String -> [Mutant] -> [Mutant]
applyRunMutantId Nothing   ms = ms
applyRunMutantId (Just mid) ms = filter (\m -> hash (_mutant m) == mid) ms

-- | Load a baseline file and filter out mutants whose hash appears in it.
applyBaseline :: Maybe FilePath -> [Mutant] -> IO [Mutant]
applyBaseline Nothing ms = return ms
applyBaseline (Just path) ms = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left e -> do
      hPutStrLn stderr $ "Warning: could not read baseline file: " ++ show e
      return ms
    Right contents -> do
      let ids = filter (not . null) (lines contents)
      return $ filter (\m -> hash (_mutant m) `notElem` ids) ms

-- | Load a blacklist file and filter out mutants whose hash appears in it.
applyBlacklist :: Maybe FilePath -> [Mutant] -> IO [Mutant]
applyBlacklist Nothing ms = return ms
applyBlacklist (Just path) ms = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left e -> do
      hPutStrLn stderr $ "Warning: could not read blacklist file: " ++ show e
      return ms
    Right contents -> do
      let ids = filter (not . null) (lines contents)
      return $ filter (\m -> hash (_mutant m) `notElem` ids) ms

-- | Parse inline @-- mucheck: disable-next-line [mutators]@ annotations.
-- Returns a list of (1-based comment line, suppressed mutator names).
-- An empty name list means suppress all mutators.
parseAnnotations :: String -> [(Int, [String])]
parseAnnotations src = concatMap check (zip [1..] (lines src))
  where
    check (n, line) =
      let trimmed = dropWhile isSpace line
      in case stripPrefix "-- mucheck: disable-next-line" trimmed of
           Nothing   -> []
           Just rest ->
             let names = case dropWhile isSpace rest of
                           "" -> []
                           s  -> splitOn ',' s
             in [(n, names)]

-- | Split a string on a delimiter character, trimming whitespace from each part.
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s  = map (dropWhile isSpace . reverse . dropWhile isSpace . reverse) $ go s ""
  where
    go []     acc = [acc]
    go (x:xs) acc
      | x == c    = acc : go xs ""
      | otherwise = go xs (acc ++ [x])

-- | Filter out mutants suppressed by inline annotations.
applyAnnotations :: [(Int, [String])] -> [Mutant] -> [Mutant]
applyAnnotations [] ms = ms
applyAnnotations anns ms = filter (not . isSuppressed) ms
  where
    isSuppressed m =
      let sl      = spanStartLine (_mspan m)
          mName   = showMuVar (_mtype m)
      in any (\(annLine, names) ->
                sl == annLine + 1 &&
                (null names || mName `elem` names)
             ) anns

-- | Write surviving mutant IDs to the update-baseline file.
writeUpdateBaseline :: Opts -> [MutantSummary] -> IO ()
writeUpdateBaseline opts tsum = case optUpdateBaseline opts of
  Nothing   -> return ()
  Just path -> do
    let aliveIds = [hash (_mutant m) | MSumAlive m _ <- tsum]
    writeFile path (unlines aliveIds)

-- | Write a compact JSON summary to the logger-json file.
writeJsonLogger :: Opts -> MAnalysisSummary -> IO ()
writeJsonLogger opts msum = case optLoggerJson opts of
  Nothing   -> return ()
  Just path -> do
    let MAnalysisSummary{..} = msum
        noerrors = _maNumMutants - _maErrors
        msiVal :: Double
        msiVal = if noerrors > 0
                 then fromIntegral _maKilled / fromIntegral noerrors
                 else 0.0
        covNoerrors = if _maCoveredNumMutants > 0
                      then _maCoveredNumMutants - _maErrors
                      else noerrors
        covMsiVal :: Double
        covMsiVal = if covNoerrors > 0
                    then fromIntegral _maKilled / fromIntegral covNoerrors
                    else 0.0
        covMsiField = if _maCoveredNumMutants > 0
                      then show covMsiVal
                      else "null"
        json = unlines
          [ "{"
          , "  \"total\": " ++ show _maNumMutants ++ ","
          , "  \"killed\": " ++ show _maKilled ++ ","
          , "  \"alive\": " ++ show _maAlive ++ ","
          , "  \"skipped\": " ++ show _maSkipped ++ ","
          , "  \"errors\": " ++ show _maErrors ++ ","
          , "  \"msi\": " ++ show msiVal ++ ","
          , "  \"covered_code_msi\": " ++ covMsiField
          , "}"
          ]
    writeFile path json

noopCheck :: FilePath -> IO ()
noopCheck file = do
  tests <- getAllTests file
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

applyExitPolicy :: Opts -> MAnalysisSummary -> IO ()
applyExitPolicy opts msum = do
  let noerrors = _maNumMutants msum - _maErrors msum
      msi | noerrors > 0 = _maKilled msum * 100 `div` noerrors
          | otherwise    = 0
      
      -- Covered MSI is calculated using the covered mutants as the baseline.
      -- Since _maNumMutants is bounded by _maCoveredNumMutants if coverage is used,
      -- we can use the same `noerrors` if it's based on _maCoveredNumMutants, but 
      -- effectively the mutants tested ARE the covered ones when a tix file is provided.
      -- If -tix is not provided, _maCoveredNumMutants is -1.
      coveredNoerrors = if _maCoveredNumMutants msum > 0 
                        then _maCoveredNumMutants msum - _maErrors msum
                        else noerrors
      coveredMsi | coveredNoerrors > 0 = _maKilled msum * 100 `div` coveredNoerrors
                 | otherwise           = 0
  
  if optIgnoreMsiNoMutations opts && _maNumMutants msum == 0 then return ()
  else do
    case optMinMsi opts of
      Just threshold | msi < threshold -> do
        putStrLn $ "MSI " ++ show msi ++ "% is below threshold " ++ show threshold ++ "%"
        exitWith (ExitFailure 5)
      _ -> return ()
      
    case optMinCoveredMsi opts of
      Just threshold | coveredMsi < threshold -> do
        putStrLn $ "Covered-MSI " ++ show coveredMsi ++ "% is below threshold " ++ show threshold ++ "%"
        exitWith (ExitFailure 5)
      _ -> return ()
  
  when (optFailOnEscape opts && _maAlive msum > 0) $ do
    putStrLn $ show (_maAlive msum) ++ " mutant(s) survived; exiting with failure"
    exitWith (ExitFailure 4)

printMutatorBreakdown :: Opts -> [MutantSummary] -> IO ()
printMutatorBreakdown _ [] = return ()
printMutatorBreakdown _opts sums = do
  let mutOf (MSumError   m _ _) = m
      mutOf (MSumSkipped m _)   = m
      mutOf (MSumAlive   m _)   = m
      mutOf (MSumKilled  m _)   = m
      mutOf (MSumOther   m _)   = m
      isKilled  (MSumKilled  _ _)   = True; isKilled  _ = False
      isAlive   (MSumAlive   _ _)   = True; isAlive   _ = False
      isErr     (MSumError   _ _ _) = True; isErr     _ = False
      isSkipped (MSumSkipped _ _)   = True; isSkipped _ = False
      mutype    = showMuVar . _mtype . mutOf
      types     = sort . nub $ map mutype sums
      row t     = let ts = filter ((== t) . mutype) sums
                      k  = length $ filter isKilled  ts
                      a  = length $ filter isAlive   ts
                      e  = length $ filter isErr     ts
                      s  = length $ filter isSkipped ts
                  in (t, k, a, e, s)
      rows      = map row types
      colW      = max 8 $ maximum $ map (\(t,_,_,_,_) -> length t) rows
      pad s'    = s' ++ replicate (colW - length s' + 2) ' '
      sep       = replicate (colW + 42) '-'
      fmtN n    = replicate (max 0 (6 - length (show n))) ' ' ++ show n
  putStrLn ""
  putStrLn $ "  " ++ pad "Mutator" ++ "  Killed   Alive  Errors  Skipped"
  putStrLn sep
  mapM_ (\(t,k,a,e,s) -> putStrLn $ "  " ++ pad t ++ "  " ++ fmtN k ++ "  " ++ fmtN a ++ "  " ++ fmtN e ++ "  " ++ fmtN s) rows
  putStrLn sep

printMutantDetails :: Opts -> String -> [MutantSummary] -> IO ()
printMutantDetails opts origSrc sums = do
  let filterStatuses s = case optOutputStatuses opts of
                           Nothing -> True
                           Just chars -> case s of
                             MSumKilled  _ _   -> 'k' `elem` chars
                             MSumAlive   _ _   -> 'a' `elem` chars
                             MSumError   _ _ _ -> 'e' `elem` chars
                             MSumSkipped _ _   -> 's' `elem` chars
                             MSumOther   _ _   -> 'k' `elem` chars
      shouldShowQuiet s = not (optQuiet opts) || case s of { MSumAlive _ _ -> True; _ -> False }
      toShow = filter (\s -> filterStatuses s && shouldShowQuiet s) sums

  forM_ toShow $ \s -> do
    let (status, Mutant{..}, logS, mErr) = case s of
                                     MSumKilled  mut l   -> ("KILLED",  mut, l, Nothing)
                                     MSumAlive   mut l   -> ("ALIVE",   mut, l, Nothing)
                                     MSumError   mut e l -> ("ERROR",   mut, l, Just e)
                                     MSumSkipped mut l   -> ("SKIPPED", mut, l, Nothing)
                                     MSumOther   mut l   -> ("OTHER",   mut, l, Nothing)
    putStrLn $ ">>> Mutant " ++ hash _mutant ++ " [" ++ status ++ "] " ++ showMuVar _mtype
    unless (optNoDiffs opts) $
      let d = unifiedDiff origSrc _mutant
      in unless (null d) $ putStr d
    when (optVerbose opts) $ do
      putStrLn "--- Full source ---"
      putStrLn _mutant
      putStrLn "-------------------"
      mapM_ print logS
    when (optDebug opts) $
      case mErr of
        Just e  -> putStrLn $ "Error: " ++ e
        Nothing -> return ()
    putStrLn ""

dryRun :: FilePath -> IO ()
-- | Produce a compact unified diff between two source strings.
-- Shows only changed lines with up to 2 lines of context.
unifiedDiff :: String -> String -> String
unifiedDiff origSrc mutSrc
  | origSrc == mutSrc = ""
  | otherwise = concatMap renderHunk hunks
  where
    oLines   = lines origSrc
    mLines   = lines mutSrc
    maxLen   = max (length oLines) (length mLines)
    ctx      = 2
    getL i ls = if i <= length ls then ls !! (i - 1) else ""
    diffIdxs = [i | i <- [1..maxLen], getL i oLines /= getL i mLines]
    ctxSet   = nub $ sort $ concatMap (\i -> [max 1 (i-ctx)..min maxLen (i+ctx)]) diffIdxs
    hunks    = groupConsec ctxSet
    renderHunk hunk@(lo:_) =
      let hi  = hunk !! (length hunk - 1)
          hdr = "@@ -" ++ show lo ++ "," ++ show (hi - lo + 1) ++ " @@\n"
          rows = concat (concatMap renderRow hunk)
      in hdr ++ rows
    renderHunk [] = ""
    renderRow i
      | i `elem` diffIdxs =
          let oL = getL i oLines; mL = getL i mLines
          in  ["-" ++ oL ++ "\n" | not (null oL) || i <= length oLines]
           ++ ["+" ++ mL ++ "\n" | not (null mL) || i <= length mLines]
      | otherwise = [" " ++ getL i oLines ++ "\n"]

-- | Group a sorted list of ints into runs of consecutive integers.
groupConsec :: [Int] -> [[Int]]
groupConsec [] = []
groupConsec (x:xs) = go [x] x xs
  where
    go cur _    []     = [cur]
    go cur prev (y:ys)
      | y == prev + 1  = go (cur ++ [y]) y ys
      | otherwise      = cur : go [y] y ys

dryRun file = do
  src <- readFile file
  let ast     = getASTFromStr src
      mutants = genMutantsFromAST defaultConfig ast
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

showMuVar :: MuVar -> String
showMuVar MutatePatternMatch          = "pattern-match"
showMuVar MutateValues                = "literal-values"
showMuVar MutateFunctions             = "functions"
showMuVar MutateNegateIfElse          = "negate-if-else"
showMuVar MutateNegateGuards          = "negate-guards"
showMuVar (MutateOther "remove-not")  = "remove-not"
showMuVar (MutateOther "remove-negation") = "remove-negation"
showMuVar (MutateOther s)             = if null s then "other" else "other:" ++ s

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
  , ""
  , "CONFIG FILE (.mucheck.yaml, auto-loaded from project root):"
  , "  min_msi: 80               Minimum required MSI (0-100)"
  , "  min_covered_msi: 80       Minimum required covered-code MSI"
  , "  timeout: 30               Per-mutant timeout in seconds"
  , "  quiet: true               Suppress killed/error output"
  , "  disable_mutators: [a, b]  Mutator names to skip"
  , "  enable_mutators: [a, b]   Restrict to named mutators"
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
