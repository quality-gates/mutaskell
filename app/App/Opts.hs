-- | CLI option types, parser, and config loader for mucheck.
-- Extracted into its own module so the spec test-suite can import and test
-- 'parseOptsFrom' without pulling in the full 'Main' module.
module App.Opts
  ( Opts(..)
  , defaultOpts
  , optsParserInfo
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
import Options.Applicative

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

-- | optparse-applicative parser for all CLI options, parameterised by a base
-- 'Opts' that supplies defaults (typically from a config file).
optsParser :: Opts -> Parser Opts
optsParser base = Opts
    <$> argument str
          ( metavar "FILE"
          <> help "Haskell source file to mutate" )
    <*> strOption
          ( long "tix" <> metavar "FILE"
          <> value (optTix base)
          <> help "HPC coverage file for coverage-guided mutation" )
    <*> flag (optDryRun base) True
          ( long "dry-run"
          <> help "Show mutation counts by type without evaluating" )
    <*> flag (optNoop base) True
          ( long "noop"
          <> help "Verify tests pass on unmodified source first (exit 3 on failure)" )
    <*> flag (optFailOnEscape base) True
          ( long "fail-on-escaped"
          <> help "Exit with code 4 if any mutant survives" )
    <*> option (Just <$> auto)
          ( long "min-msi" <> metavar "PCT"
          <> value (optMinMsi base)
          <> help "Exit with code 5 if MSI is below PCT percent" )
    <*> option (Just <$> auto)
          ( long "min-covered-msi" <> metavar "PCT"
          <> value (optMinCoveredMsi base)
          <> help "Exit with code 5 if covered-code MSI is below PCT (requires --tix)" )
    <*> ((optDisable base ++) <$> many
          ( strOption
            ( long "disable" <> metavar "NAME"
            <> help "Skip mutants of the named type (repeatable)" )))
    <*> ((optEnable base ++) <$> many
          ( strOption
            ( long "enable" <> metavar "NAME"
            <> help "Run only mutants of the named type (repeatable)" )))
    <*> option (Just <$> str)
          ( long "config" <> metavar "FILE"
          <> value (optConfig base)
          <> help "Load config from FILE instead of .mucheck.yaml" )
    <*> flag (optQuiet base) True
          ( long "quiet"
          <> help "Show only surviving mutants; suppress killed/error output" )
    <*> flag (optVerbose base) True
          ( long "verbose"
          <> help "Print full mutant source and test output during evaluation" )
    <*> flag (optDebug base) True
          ( long "debug"
          <> help "Print stable IDs and raw interpreter diagnostics" )
    <*> flag (optNoDiffs base) True
          ( long "no-diffs"
          <> help "Suppress per-mutant unified diff output" )
    <*> flag (optIgnoreMsiNoMutations base) True
          ( long "ignore-msi-with-no-mutations"
          <> help "Pass quality gates when no mutations are generated" )
    <*> option (Just <$> str)
          ( long "output-statuses" <> metavar "CHARS"
          <> value (optOutputStatuses base)
          <> help "Show only result types matching chars: k=killed a=alive e=error s=skip" )
    <*> option (Just <$> auto)
          ( long "timeout" <> metavar "N"
          <> value (optTimeout base)
          <> help "Per-mutant timeout in seconds" )
    <*> option (Just <$> str)
          ( long "logger-json" <> metavar "FILE"
          <> value (optLoggerJson base)
          <> help "Write a compact JSON run summary to FILE" )
    <*> option (Just <$> str)
          ( long "baseline" <> metavar "FILE"
          <> value (optBaseline base)
          <> help "Skip mutants whose ID appears in FILE from a previous run" )
    <*> option (Just <$> str)
          ( long "update-baseline" <> metavar "FILE"
          <> value (optUpdateBaseline base)
          <> help "Write surviving mutant IDs to FILE after the run" )
    <*> option (Just <$> str)
          ( long "blacklist" <> metavar "FILE"
          <> value (optBlacklist base)
          <> help "Suppress mutations whose ID appears in FILE" )
    <*> option (Just <$> str)
          ( long "run-mutant-id" <> metavar "ID"
          <> value (optRunMutantId base)
          <> help "Evaluate only the mutant with the given stable ID"
          <> internal )
    <*> option (Just <$> str)
          ( long "logger-github" <> metavar "FILE"
          <> value (optLoggerGithub base)
          <> help "Write GitHub Actions annotations for escaped mutants to FILE" )
    <*> option (Just <$> str)
          ( long "logger-gitlab" <> metavar "FILE"
          <> value (optLoggerGitlab base)
          <> help "Write GitLab Code Quality JSON for escaped mutants to FILE" )
    <*> option (Just <$> auto)
          ( long "timeout-coefficient" <> metavar "N"
          <> value (optTimeoutCoef base)
          <> help "Set timeout to N x measured baseline test-suite runtime" )
    <*> option (Just <$> str)
          ( long "git-diff-base" <> metavar "REF"
          <> value (optGitDiffBase base)
          <> help "Skip mutation if file is not in 'git diff --name-only REF'" )
    <*> flag (optGitDiffLines base) True
          ( long "git-diff-lines"
          <> help "Restrict mutants to changed lines (requires --git-diff-base)" )
    <*> option (Just <$> str)
          ( long "keep-mutants" <> metavar "DIR"
          <> value (optKeepMutants base)
          <> help "Write mutant files to DIR and keep them after evaluation" )
    <*> option (Just <$> str)
          ( long "logger-agentic-json" <> metavar "FILE"
          <> value (optLoggerAgenticJson base)
          <> help "Write per-mutant JSON for LLM consumption to FILE" )
    <*> option (Just <$> str)
          ( long "logger-html" <> metavar "FILE"
          <> value (optLoggerHtml base)
          <> help "Write a standalone HTML mutation report to FILE" )
    <*> ((optTestArgs base ++) <$> many
          ( strOption
            ( long "test-args" <> metavar "ARG"
            <> help "Pass ARG to the test runner (repeatable)" )))
    <*> flag (optCoverage base) True
          ( long "coverage"
          <> help "Auto-discover a .tix file in the current directory" )
    <*> pure (optSilent base)
    <*> pure (optMaxMutants base)
    <*> pure (optIgnoreLines base)
    <*> pure (optSkipWithoutTest base)
    <*> pure (optExcludeDirs base)
    <*> option auto
          ( long "workers" <> metavar "N"
          <> value (optWorkers base)
          <> help "Number of parallel worker processes (default: 1)" )
    <*> option (Just <$> str)
          ( long "worker-output" <> metavar "FILE"
          <> value (optWorkerOutput base)
          <> internal )

-- | 'ParserInfo' wrapping 'optsParser'.  Use with 'execParser' in 'Main' or
-- 'execParserPure' in tests.
optsParserInfo :: Opts -> ParserInfo Opts
optsParserInfo base = info (optsParser base <**> helper)
    ( fullDesc
    <> progDesc "Mutation testing for Haskell source files"
    <> header "mucheck - automated mutation testing"
    <> footer (unlines
        [ "Mutator names (for --disable / --enable):"
        , "  pattern-match  literal-values  functions"
        , "  negate-if-else  negate-guards  remove-not  remove-negation"
        , "  Trailing '*' is a prefix wildcard, e.g. 'other:*'"
        , ""
        , "Exit codes:"
        , "  0  Tests ran; no quality gate triggered"
        , "  2  Bad arguments"
        , "  3  Pre-flight failure (--noop: tests fail on original source)"
        , "  4  Escaped mutants (--fail-on-escaped)"
        , "  5  MSI below threshold (--min-msi / --min-covered-msi)"
        ]) )

-- | Parse CLI args into 'Opts', starting from 'defaultOpts'.
parseOpts :: [String] -> Either String Opts
parseOpts = parseOptsFrom defaultOpts

-- | Parse CLI args into 'Opts', starting from a given base.
-- Returns 'Left' with a human-readable error on bad input.
parseOptsFrom :: Opts -> [String] -> Either String Opts
parseOptsFrom base args =
    case execParserPure defaultPrefs (optsParserInfo base) args of
        Success opts        -> validateOpts opts
        Failure f           -> Left $ fst (renderFailure f "mucheck")
        CompletionInvoked _ -> Left "completion requested"

-- | Post-parse validation: reject mutually exclusive flag combinations.
validateOpts :: Opts -> Either String Opts
validateOpts opts
  | not (null (optEnable opts)) && not (null (optDisable opts))
  = Left "Cannot use --enable and --disable together; use one or the other"
  | otherwise
  = Right opts
