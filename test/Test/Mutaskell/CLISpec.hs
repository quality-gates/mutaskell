module Test.Mutaskell.CLISpec where

import Test.Hspec
import App.Opts
    ( Opts(..)
    , defaultOpts
    , parseOptsFrom
    , parseYamlConfigStr
    )

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

