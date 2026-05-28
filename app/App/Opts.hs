{-# LANGUAGE RecordWildCards #-}
-- | CLI option types, parser, and config loader for mucheck.
-- Extracted into its own module so the spec test-suite can import and test
-- 'parseOptsFrom' without pulling in the full 'Main' module.
module App.Opts
  ( Opts(..)
  , defaultOpts
  , parseOpts
  , parseOptsFrom
  , validateOpts
  , loadConfig
  , parseYamlKV
  , parseYamlList
  , applyYamlConfig
  , knownConfigKeys
  , splitOn
  ) where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (intercalate)

-- | All command-line options for a mucheck run.
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
  , optExcludeDirs  :: [String]
  , optWorkers      :: Int
  , optWorkerOutput :: Maybe FilePath
  } deriving (Eq, Show)

-- | Default options: no flags set, no file.
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
  , optExcludeDirs  = []
  , optWorkers      = 1
  , optWorkerOutput = Nothing
  }

-- | Known YAML config file keys.
knownConfigKeys :: [String]
knownConfigKeys =
  [ "min_msi", "min_covered_msi", "timeout", "quiet", "silent_mode"
  , "max_mutants", "json_output", "html_output"
  , "disable_mutators", "enable_mutators"
  , "ignore_source_lines", "skip_without_test"
  , "exclude_dirs", "workers"
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

-- | Parse a YAML-ish key:value file into a list of pairs.
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

-- | Apply a list of YAML key-value pairs to an 'Opts' record.
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
    applyPair o ("exclude_dirs", v) =
      o { optExcludeDirs = optExcludeDirs o ++ parseYamlList v }
    applyPair o ("workers", v) = case (reads v, optWorkers o) of
      ([(i, "")], 1) -> o { optWorkers = i }
      _              -> o
    applyPair o _ = o
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Parse a YAML-ish list value: either @[a, b, c]@ or a bare string.
parseYamlList :: String -> [String]
parseYamlList s = case dropWhile isSpace s of
  ('[' : rest) ->
    let inner = takeWhile (/= ']') rest
    in map trim (splitOn ',' inner)
  s' -> let t = trim s' in if null t then [] else [t]
  where trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Split a string on a delimiter character, trimming whitespace from each token.
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s  = map (dropWhile isSpace . reverse . dropWhile isSpace . reverse) $ go s ""
  where
    go []     acc = [acc]
    go (x:xs) acc
      | x == c    = acc : go xs ""
      | otherwise = go xs (acc ++ [x])

-- | Parse CLI args into 'Opts', starting from 'defaultOpts'.
parseOpts :: [String] -> Either String Opts
parseOpts = parseOptsFrom defaultOpts

-- | Parse CLI args into 'Opts', starting from a given base.
-- Returns 'Left' with a human-readable error on bad input.
parseOptsFrom :: Opts -> [String] -> Either String Opts
parseOptsFrom base args = go base args >>= validateOpts
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
    go _    ("--workers"        : [])   = Left "--workers requires an integer argument"
    go opts ("--workers" : n    : rest) =
      case reads n of
        [(i, "")] -> go opts { optWorkers = i } rest
        _         -> Left $ "--workers requires an integer argument, got: " ++ n
    go _    ("--worker-output"   : [])   = Left "--worker-output requires a file path argument"
    go opts ("--worker-output" : f : rest) = go opts { optWorkerOutput = Just f } rest
    go _    (arg@('-' : _)       : _)    = Left $ "Unknown flag: " ++ arg
    go opts (file                : _)    = Right opts { optFile = file }
    go _    []                           = Left "Need a file argument"

-- | Post-parse validation: reject combinations of flags that are mutually exclusive.
validateOpts :: Opts -> Either String Opts
validateOpts opts
  | not (null (optEnable opts)) && not (null (optDisable opts))
  = Left "Cannot use --enable and --disable together; use one or the other"
  | otherwise
  = Right opts
