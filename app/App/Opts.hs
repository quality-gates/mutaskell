{-# LANGUAGE OverloadedStrings #-}
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
  , parseYamlConfigStr
  , splitOn
  ) where

import Data.ByteString.Char8 (pack)
import System.Directory (doesFileExist)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Options.Applicative
import Options.Applicative.Help.Pretty (Doc, vsep, pretty)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), withObject, (.:?))

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

-- Private list of valid config keys; used for unknown-key rejection.
knownYamlKeys :: [String]
knownYamlKeys =
  [ "min_msi", "min_covered_msi", "timeout", "quiet", "silent_mode"
  , "max_mutants", "json_output", "html_output"
  , "disable_mutators", "enable_mutators"
  , "ignore_source_lines", "skip_without_test"
  , "exclude_dirs", "workers"
  ]

-- | Typed representation of the @.mucheck.yaml@ config file.
data YamlConfig = YamlConfig
  { ycMinMsi            :: Maybe Int
  , ycMinCoveredMsi     :: Maybe Int
  , ycTimeout           :: Maybe Int
  , ycMaxMutants        :: Maybe Int
  , ycQuiet             :: Maybe Bool
  , ycSilentMode        :: Maybe Bool
  , ycJsonOutput        :: Maybe String
  , ycHtmlOutput        :: Maybe String
  , ycDisableMutators   :: Maybe [String]
  , ycEnableMutators    :: Maybe [String]
  , ycIgnoreSourceLines :: Maybe [String]
  , ycSkipWithoutTest   :: Maybe Bool
  , ycExcludeDirs       :: Maybe [String]
  , ycWorkers           :: Maybe Int
  }

instance FromJSON YamlConfig where
    parseJSON = withObject "config" $ \obj -> do
        let unknowns = filter (\k -> Key.toString k `notElem` knownYamlKeys) (KM.keys obj)
        case unknowns of
            (k:_) -> fail $ "Unknown config key: " ++ Key.toString k
                              ++ ". Known keys: " ++ intercalate ", " knownYamlKeys
            [] -> YamlConfig
                    <$> obj .:? "min_msi"
                    <*> obj .:? "min_covered_msi"
                    <*> obj .:? "timeout"
                    <*> obj .:? "max_mutants"
                    <*> obj .:? "quiet"
                    <*> obj .:? "silent_mode"
                    <*> obj .:? "json_output"
                    <*> obj .:? "html_output"
                    <*> obj .:? "disable_mutators"
                    <*> obj .:? "enable_mutators"
                    <*> obj .:? "ignore_source_lines"
                    <*> obj .:? "skip_without_test"
                    <*> obj .:? "exclude_dirs"
                    <*> obj .:? "workers"

-- | Apply a parsed 'YamlConfig' to an 'Opts' record.
-- Config values fill in defaults; CLI flags (applied later) override these.
applyYamlConfigRecord :: YamlConfig -> Opts -> Opts
applyYamlConfigRecord cfg opts = opts
    { optMinMsi         = ycMinMsi cfg          <|> optMinMsi opts
    , optMinCoveredMsi  = ycMinCoveredMsi cfg   <|> optMinCoveredMsi opts
    , optTimeout        = ycTimeout cfg         <|> optTimeout opts
    , optMaxMutants     = ycMaxMutants cfg      <|> optMaxMutants opts
    , optQuiet          = fromMaybe (optQuiet opts)          (ycQuiet cfg)
    , optSilent         = fromMaybe (optSilent opts)         (ycSilentMode cfg)
    , optLoggerJson     = ycJsonOutput cfg      <|> optLoggerJson opts
    , optLoggerHtml     = ycHtmlOutput cfg      <|> optLoggerHtml opts
    , optDisable        = optDisable opts        ++ fromMaybe [] (ycDisableMutators cfg)
    , optEnable         = optEnable opts         ++ fromMaybe [] (ycEnableMutators cfg)
    , optIgnoreLines    = optIgnoreLines opts    ++ fromMaybe [] (ycIgnoreSourceLines cfg)
    , optSkipWithoutTest = fromMaybe (optSkipWithoutTest opts) (ycSkipWithoutTest cfg)
    , optExcludeDirs    = optExcludeDirs opts    ++ fromMaybe [] (ycExcludeDirs cfg)
    , optWorkers        = fromMaybe (optWorkers opts) (ycWorkers cfg)
    }

-- | Load config and return either an error string or an 'Opts' transformer.
-- Applied to 'defaultOpts' before CLI parsing so CLI flags override config.
-- Returns @Right id@ if the file does not exist.
--
-- Note: 'Yaml.decodeFileEither' catches 'IOException' internally and wraps it
-- as a 'ParseException', so a plain @try@ cannot distinguish missing-file from
-- a genuine parse error.  We therefore check existence first.
loadConfig :: Maybe FilePath -> IO (Either String (Opts -> Opts))
loadConfig mPath = do
    let path = fromMaybe ".mucheck.yaml" mPath
    exists <- doesFileExist path
    if not exists
        then return (Right id)
        else do
            result <- Yaml.decodeFileEither path
            case result of
                Left err  -> return (Left (Yaml.prettyPrintParseException err))
                Right cfg -> return (Right (applyYamlConfigRecord cfg))

-- | Parse a YAML config string (for testing) and return an 'Opts' transformer.
parseYamlConfigStr :: String -> Either String (Opts -> Opts)
parseYamlConfigStr s = case Yaml.decodeEither' (pack s) of
    Left err  -> Left (Yaml.prettyPrintParseException err)
    Right cfg -> Right (applyYamlConfigRecord cfg)

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
    <> footerDoc (Just footerText) )

footerText :: Doc
footerText = vsep (map pretty
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
    ])

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
