module Test.Mutaskell.CLISpec where

import System.Exit (ExitCode (..))
import System.IO (writeFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

import App.Opts
    ( Opts(..)
    , defaultOpts
    , extractConfigArg
    , parseOptsFrom
    , parseYamlConfigStr
    )
import Test.Mutaskell.WorkerSpec (findMucheckBin)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parseOptsFrom" $ do
        it "rejects --enable and --disable used together" $ do
            let result = parseOptsFrom defaultOpts
                          ["--enable", "functions", "--disable", "literal-values", "SomeFile.hs"]
            result `shouldBe` Left "Cannot use --enable and --disable together; use one or the other"

        it "accepts --enable alone" $ do
            let result = parseOptsFrom defaultOpts ["--enable", "functions", "SomeFile.hs"]
            case result of
                Right _  -> return ()
                Left err -> expectationFailure $ "Expected Right but got Left: " ++ err

        it "accepts --disable alone" $ do
            let result = parseOptsFrom defaultOpts ["--disable", "literal-values", "SomeFile.hs"]
            case result of
                Right _  -> return ()
                Left err -> expectationFailure $ "Expected Right but got Left: " ++ err

        it "rejects --disable and --enable in any order" $ do
            let result = parseOptsFrom defaultOpts
                          ["--disable", "literal-values", "--enable", "functions", "SomeFile.hs"]
            result `shouldBe` Left "Cannot use --enable and --disable together; use one or the other"

        it "rejects --min-covered-msi without --tix or --coverage, naming both flags" $ do
            let result = parseOptsFrom defaultOpts ["--min-covered-msi", "50", "F.hs"]
            case result of
                Left err -> do
                    err `shouldContain` "--min-covered-msi"
                    err `shouldContain` "--tix"
                Right _ -> expectationFailure "Expected Left but got Right"

        it "accepts --min-covered-msi with --tix" $ do
            let result = parseOptsFrom defaultOpts
                          ["--min-covered-msi", "50", "--tix", "c.tix", "F.hs"]
            fmap optMinCoveredMsi result `shouldBe` Right (Just 50)

        it "accepts --min-covered-msi with --coverage" $ do
            let result = parseOptsFrom defaultOpts
                          ["--min-covered-msi", "50", "--coverage", "F.hs"]
            fmap optMinCoveredMsi result `shouldBe` Right (Just 50)

        it "returns Left for unknown flags" $ do
            let result = parseOptsFrom defaultOpts ["--unknown-flag", "SomeFile.hs"]
            case result of
                Left _  -> return ()
                Right _ -> expectationFailure "Expected Left but got Right"

        it "returns Left when no file argument is given" $ do
            let result = parseOptsFrom defaultOpts []
            case result of
                Left _  -> return ()
                Right _ -> expectationFailure "Expected Left but got Right"

        -- Flag round-trip tests (a): each flag sets the expected Opts field
        it "--dry-run sets optDryRun" $ do
            let result = parseOptsFrom defaultOpts ["--dry-run", "F.hs"]
            fmap optDryRun result `shouldBe` Right True

        it "--noop sets optNoop" $ do
            let result = parseOptsFrom defaultOpts ["--noop", "F.hs"]
            fmap optNoop result `shouldBe` Right True

        it "--fail-on-escaped sets optFailOnEscape" $ do
            let result = parseOptsFrom defaultOpts ["--fail-on-escaped", "F.hs"]
            fmap optFailOnEscape result `shouldBe` Right True

        it "--quiet sets optQuiet" $ do
            let result = parseOptsFrom defaultOpts ["--quiet", "F.hs"]
            fmap optQuiet result `shouldBe` Right True

        it "--verbose sets optVerbose" $ do
            let result = parseOptsFrom defaultOpts ["--verbose", "F.hs"]
            fmap optVerbose result `shouldBe` Right True

        it "--no-diffs sets optNoDiffs" $ do
            let result = parseOptsFrom defaultOpts ["--no-diffs", "F.hs"]
            fmap optNoDiffs result `shouldBe` Right True

        it "--workers sets optWorkers" $ do
            let result = parseOptsFrom defaultOpts ["--workers", "4", "F.hs"]
            fmap optWorkers result `shouldBe` Right 4

        it "--min-msi sets optMinMsi" $ do
            let result = parseOptsFrom defaultOpts ["--min-msi", "80", "F.hs"]
            fmap optMinMsi result `shouldBe` Right (Just 80)

        it "--timeout sets optTimeout" $ do
            let result = parseOptsFrom defaultOpts ["--timeout", "30", "F.hs"]
            fmap optTimeout result `shouldBe` Right (Just 30)

        it "--logger-json sets optLoggerJson" $ do
            let result = parseOptsFrom defaultOpts ["--logger-json", "out.json", "F.hs"]
            fmap optLoggerJson result `shouldBe` Right (Just "out.json")

        it "--tix sets optTix" $ do
            let result = parseOptsFrom defaultOpts ["--tix", "cov.tix", "F.hs"]
            fmap optTix result `shouldBe` Right "cov.tix"

        -- Orchestrator / project-mode flags
        it "--exec sets optExec" $ do
            let result = parseOptsFrom defaultOpts ["--exec", "F.hs"]
            fmap optExec result `shouldBe` Right True

        it "--build-cmd sets optBuildCmd" $ do
            let result = parseOptsFrom defaultOpts ["--build-cmd", "cabal build all", "F.hs"]
            fmap optBuildCmd result `shouldBe` Right (Just "cabal build all")

        it "--test-cmd sets optTestCmd" $ do
            let result = parseOptsFrom defaultOpts ["--test-cmd", "cabal test all", "F.hs"]
            fmap optTestCmd result `shouldBe` Right (Just "cabal test all")

        it "--max-mutants sets optMaxMutants" $ do
            let result = parseOptsFrom defaultOpts ["--max-mutants", "50", "F.hs"]
            fmap optMaxMutants result `shouldBe` Right (Just 50)

        it "--time-budget sets optTimeBudget" $ do
            let result = parseOptsFrom defaultOpts ["--time-budget", "1800", "F.hs"]
            fmap optTimeBudget result `shouldBe` Right (Just 1800)

        it "--jobs sets optJobs" $ do
            let result = parseOptsFrom defaultOpts ["--jobs", "4", "F.hs"]
            fmap optJobs result `shouldBe` Right 4

        it "--run-mutant-workload sets optRunMutantWorkload" $ do
            let result = parseOptsFrom defaultOpts ["--run-mutant-workload", "wl.json", "F.hs"]
            fmap optRunMutantWorkload result `shouldBe` Right (Just "wl.json")

        it "defaults: optRunMutantWorkload is Nothing" $
            optRunMutantWorkload defaultOpts `shouldBe` Nothing

        it "defaults: optJobs is 1, optExec is False, optMaxMutants is Nothing" $ do
            optJobs defaultOpts `shouldBe` 1
            optExec defaultOpts `shouldBe` False
            optMaxMutants defaultOpts `shouldBe` Nothing

        it "--jobs with non-integer returns Left" $ do
            let result = parseOptsFrom defaultOpts ["--jobs", "lots", "F.hs"]
            case result of
                Left _  -> return ()
                Right _ -> expectationFailure "Expected Left but got Right"

        it "--max-mutants with non-integer returns Left" $ do
            let result = parseOptsFrom defaultOpts ["--max-mutants", "many", "F.hs"]
            case result of
                Left _  -> return ()
                Right _ -> expectationFailure "Expected Left but got Right"

        -- Error cases (b, c): bad arguments
        it "--min-msi with non-integer returns Left" $ do
            let result = parseOptsFrom defaultOpts ["--min-msi", "notanint", "F.hs"]
            case result of
                Left _  -> return ()
                Right _ -> expectationFailure "Expected Left but got Right"

        it "--timeout with non-integer returns Left" $ do
            let result = parseOptsFrom defaultOpts ["--timeout", "abc", "F.hs"]
            case result of
                Left _  -> return ()
                Right _ -> expectationFailure "Expected Left but got Right"

        it "--workers with non-integer returns Left" $ do
            let result = parseOptsFrom defaultOpts ["--workers", "two", "F.hs"]
            case result of
                Left _  -> return ()
                Right _ -> expectationFailure "Expected Left but got Right"

        -- Config loader tests (d): config values applied as defaults; CLI overrides
        it "config file min_msi is applied as default" $ do
            case parseYamlConfigStr "min_msi: 60" of
                Left err -> expectationFailure $ "Parse error: " ++ err
                Right fn -> optMinMsi (fn defaultOpts) `shouldBe` Just 60

        it "CLI --min-msi overrides config file value" $ do
            case parseYamlConfigStr "min_msi: 60" of
                Left err -> expectationFailure $ "Parse error: " ++ err
                Right fn -> do
                    let base = fn defaultOpts
                    case parseOptsFrom base ["--min-msi", "90", "F.hs"] of
                        Right opts -> optMinMsi opts `shouldBe` Just 90
                        Left err'  -> expectationFailure $ "Expected Right but got Left: " ++ err'

        it "config file quiet: true is applied" $ do
            case parseYamlConfigStr "quiet: true" of
                Left err -> expectationFailure $ "Parse error: " ++ err
                Right fn -> optQuiet (fn defaultOpts) `shouldBe` True

        it "config file workers is applied" $ do
            case parseYamlConfigStr "workers: 3" of
                Left err -> expectationFailure $ "Parse error: " ++ err
                Right fn -> optWorkers (fn defaultOpts) `shouldBe` 3

        it "config YAML inline list for disable_mutators is parsed" $ do
            case parseYamlConfigStr "disable_mutators: [functions, literal-values]" of
                Left err -> expectationFailure $ "Parse error: " ++ err
                Right fn -> optDisable (fn defaultOpts) `shouldBe` ["functions", "literal-values"]

        it "config YAML block list for disable_mutators is parsed" $ do
            case parseYamlConfigStr "disable_mutators:\n  - functions\n  - literal-values" of
                Left err -> expectationFailure $ "Parse error: " ++ err
                Right fn -> optDisable (fn defaultOpts) `shouldBe` ["functions", "literal-values"]

        it "config unknown key is rejected with error" $ do
            case parseYamlConfigStr "unknown_key: foo" of
                Left _  -> return ()
                Right _ -> expectationFailure "Expected Left for unknown key"

    -- A missing source file must be a CLI argument error (exit 2, one-line
    -- message), not an uncaught IOException with a GHC call stack (issue #78).
    describe "missing source file" $ do
        it "plain run exits 2 with a readable error, not an uncaught IOException" $ do
            bin <- findMucheckBin
            case bin of
                Nothing -> pendingWith "mucheck binary not built (run cabal build all)"
                Just exe -> do
                    (ec, out, errOut) <-
                        readProcessWithExitCode exe ["/no/such/MutaskellFile.hs"] ""
                    ec `shouldBe` ExitFailure 2
                    (out ++ errOut) `shouldContain` "file not found"
                    (out ++ errOut) `shouldNotContain` "Uncaught exception"

        it "--dry-run exits 2 with a readable error, not an uncaught IOException" $ do
            bin <- findMucheckBin
            case bin of
                Nothing -> pendingWith "mucheck binary not built (run cabal build all)"
                Just exe -> do
                    (ec, out, errOut) <- readProcessWithExitCode exe
                        ["--dry-run", "/no/such/MutaskellFile.hs"] ""
                    ec `shouldBe` ExitFailure 2
                    (out ++ errOut) `shouldContain` "file not found"
                    (out ++ errOut) `shouldNotContain` "Uncaught exception"

    describe "extractConfigArg" $ do
        it "extracts config file with space syntax" $ do
            extractConfigArg ["--config", "myconfig.yaml"] `shouldBe` Just "myconfig.yaml"

        it "extracts config file with equals syntax" $ do
            extractConfigArg ["--config=myconfig.yaml"] `shouldBe` Just "myconfig.yaml"

        it "extracts config file with equals syntax among other arguments" $ do
            extractConfigArg ["--dry-run", "--config=myconfig.yaml", "File.hs"] `shouldBe` Just "myconfig.yaml"

        it "extracts config file with space syntax among other arguments" $ do
            extractConfigArg ["--dry-run", "--config", "myconfig.yaml", "File.hs"] `shouldBe` Just "myconfig.yaml"

        it "returns Nothing when no --config flag is given" $ do
            extractConfigArg ["--dry-run", "File.hs"] `shouldBe` Nothing

        it "returns Nothing for empty args" $ do
            extractConfigArg [] `shouldBe` Nothing

        it "returns Nothing when --config has no following argument" $ do
            extractConfigArg ["--config"] `shouldBe` Nothing

        it "extracts empty string when --config=" $ do
            extractConfigArg ["--config="] `shouldBe` Just ""

    describe "--config=FILE syntax" $ do
        it "enforces configuration quality gates when passed as --config=FILE" $ do
            bin <- findMucheckBin
            case bin of
                Nothing -> pendingWith "mucheck binary not built (run cabal build all)"
                Just exe -> withSystemTempDirectory "mutaskell-test" $ \tmpDir -> do
                    let cfgFile = tmpDir ++ "/quality-gate.yaml"
                    writeFile cfgFile "min_covered_msi: 50\n"
                    (ec, out, errOut) <- readProcessWithExitCode exe
                        ["--config=" ++ cfgFile, "Examples/AssertCheckTest.hs", "--dry-run"] ""
                    ec `shouldBe` ExitFailure 2
                    (out ++ errOut) `shouldContain` "--min-covered-msi requires coverage data"

        it "reports config parse error when passed as --config=FILE" $ do
            bin <- findMucheckBin
            case bin of
                Nothing -> pendingWith "mucheck binary not built (run cabal build all)"
                Just exe -> withSystemTempDirectory "mutaskell-test" $ \tmpDir -> do
                    let cfgFile = tmpDir ++ "/invalid.yaml"
                    writeFile cfgFile "unknown_key: true\n"
                    (ec, out, errOut) <- readProcessWithExitCode exe
                        ["--config=" ++ cfgFile, "Examples/AssertCheckTest.hs", "--dry-run"] ""
                    ec `shouldBe` ExitFailure 2
                    (out ++ errOut) `shouldContain` "Unknown config key"


