module Test.Mutaskell.AnalysisSummarySpec (main, spec) where

import Control.Exception (try)
import Data.List (isInfixOf)
import System.Exit (ExitCode(..))
import Test.Hspec
import App.Exit (applyExitPolicy)
import App.Opts (Opts(..), defaultOpts)
import Test.Mutaskell.AnalysisSummary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Semigroup and Monoid instances" $ do
        it "satisfies left identity" $ do
            let s = MAnalysisSummary 10 10 2 8 0 0
            mempty <> s `shouldBe` s

        it "satisfies right identity" $ do
            let s = MAnalysisSummary 10 10 2 8 0 0
            s <> mempty `shouldBe` s

        it "combines counts correctly" $ do
            let s1 = MAnalysisSummary 5 10 1 7 2 0
                s2 = MAnalysisSummary 5 10 3 6 1 1
                combined = s1 <> s2
            combined `shouldBe` MAnalysisSummary 10 20 4 13 3 1

        it "preserves uncovered status (-1) when both are uncovered" $ do
            let s1 = MAnalysisSummary (-1) 5 1 4 0 0
                s2 = MAnalysisSummary (-1) 5 2 3 0 0
            _maCoveredNumMutants (s1 <> s2) `shouldBe` (-1)

    describe "summary metrics" $ do
        it "calculates summaryTotal as the number of evaluated mutants" $ do
            summaryTotal (MAnalysisSummary (-1) 10 0 0 0 0) `shouldBe` 10
            summaryTotal (MAnalysisSummary 20 10 0 0 0 0) `shouldBe` 10

        it "calculates summaryNoErrors as evaluated mutants minus errors" $ do
            summaryNoErrors (MAnalysisSummary (-1) 10 0 8 2 0) `shouldBe` 8
            summaryNoErrors (MAnalysisSummary 20 10 0 8 2 0) `shouldBe` 8

        it "calculates summaryMsi correctly" $ do
            let s = MAnalysisSummary (-1) 10 2 8 0 0
            summaryMsi s `shouldBe` 80

        it "calculates summaryMsi against evaluated sampled mutants even when covered mutant count is larger" $ do
            let s = MAnalysisSummary 200 100 0 100 0 0
            summaryMsi s `shouldBe` 100
            summaryCoveredMsi s `shouldBe` Just 50

        it "returns 0 MSI when all mutants resulted in errors" $ do
            let s = MAnalysisSummary (-1) 5 0 0 5 0
            summaryMsi s `shouldBe` 0

        it "calculates summaryCoveredMsi when coverage is present" $ do
            let s = MAnalysisSummary 10 10 2 8 0 0
            summaryCoveredMsi s `shouldBe` Just 80

        it "returns Nothing for summaryCoveredMsi when coverage is not present (-1)" $ do
            let s = MAnalysisSummary (-1) 10 2 8 0 0
            summaryCoveredMsi s `shouldBe` Nothing

        it "returns Just 0 for summaryCoveredMsi when coverage is present but covers zero mutants" $ do
            let s = MAnalysisSummary 0 10 0 10 0 0
            summaryCoveredMsi s `shouldBe` Just 0

        it "includes Covered code MSI: 0% when coverage is present but covers zero mutants" $ do
            let s = MAnalysisSummary 0 10 0 10 0 0
            show s `shouldSatisfy` ("Covered code MSI: 0%" `isInfixOf`)

    describe "Show instance" $ do
        it "produces formatted output containing MSI, Total mutants, and percentages" $ do
            let s = MAnalysisSummary (-1) 10 2 8 0 0
                out = show s
            out `shouldSatisfy` ("Total mutants:" `isInfixOf`)
            out `shouldSatisfy` ("Killed:" `isInfixOf`)
            out `shouldSatisfy` ("%" `isInfixOf`)

        it "includes Covered code MSI when coverage is present" $ do
            let s = MAnalysisSummary 200 100 0 100 0 0
                out = show s
            out `shouldSatisfy` ("Mutation score (MSI): 100%" `isInfixOf`)
            out `shouldSatisfy` ("Covered code MSI: 50%" `isInfixOf`)

        it "formats a zero-mutant summary with (0%) percentages" $ do
            let out = show (mempty :: MAnalysisSummary)
            out `shouldSatisfy` ("Errors: 0  (0%)" `isInfixOf`)
            out `shouldSatisfy` ("Killed: 0/0 (0%)" `isInfixOf`)

    describe "applyExitPolicy" $ do
        it "evaluates --min-msi against evaluated mutant MSI and passes when MSI meets threshold" $ do
            let s = MAnalysisSummary 200 100 0 100 0 0
                opts = defaultOpts { optMinMsi = Just 80 }
            res <- try (applyExitPolicy opts s) :: IO (Either ExitCode ())
            res `shouldBe` Right ()

        it "evaluates --min-covered-msi against covered code MSI and exits with failure when below threshold" $ do
            let s = MAnalysisSummary 200 100 0 100 0 0
                opts = defaultOpts { optMinCoveredMsi = Just 80 }
            res <- try (applyExitPolicy opts s) :: IO (Either ExitCode ())
            res `shouldBe` Left (ExitFailure 5)

        it "passes --min-covered-msi when covered code MSI meets threshold" $ do
            let s = MAnalysisSummary 200 100 0 100 0 0
                opts = defaultOpts { optMinCoveredMsi = Just 50 }
            res <- try (applyExitPolicy opts s) :: IO (Either ExitCode ())
            res `shouldBe` Right ()

        it "fails --min-covered-msi when coverage is present but covers zero mutants" $ do
            let s = MAnalysisSummary 0 10 0 10 0 0
                opts = defaultOpts { optMinCoveredMsi = Just 70 }
            res <- try (applyExitPolicy opts s) :: IO (Either ExitCode ())
            res `shouldBe` Left (ExitFailure 5)

        it "rejects --min-covered-msi with an error when coverage data is absent (-1)" $ do
            let s = MAnalysisSummary (-1) 10 0 10 0 0
                opts = defaultOpts { optMinCoveredMsi = Just 70 }
            res <- try (applyExitPolicy opts s) :: IO (Either ExitCode ())
            res `shouldBe` Left (ExitFailure 2)

