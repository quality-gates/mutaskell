module Test.MuCheck.CLISpec where

import Test.Hspec
import App.Opts (parseOptsFrom, defaultOpts)

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
                Left err -> err `shouldBe` "Unknown flag: --unknown-flag"
                Right _  -> expectationFailure "Expected Left but got Right"

        it "returns Left when no file argument is given" $ do
            let result = parseOptsFrom defaultOpts []
            result `shouldBe` Left "Need a file argument"
