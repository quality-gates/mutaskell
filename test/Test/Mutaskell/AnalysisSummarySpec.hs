module Test.Mutaskell.AnalysisSummarySpec (main, spec) where

import Data.List (isInfixOf)
import Test.Hspec
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
        it "calculates summaryTotal as the maximum of covered and sampled" $ do
            summaryTotal (MAnalysisSummary (-1) 10 0 0 0 0) `shouldBe` 10
            summaryTotal (MAnalysisSummary 20 10 0 0 0 0) `shouldBe` 20

        it "calculates summaryNoErrors as total minus errors" $ do
            summaryNoErrors (MAnalysisSummary (-1) 10 0 8 2 0) `shouldBe` 8

        it "calculates summaryMsi correctly" $ do
            let s = MAnalysisSummary (-1) 10 2 8 0 0
            summaryMsi s `shouldBe` 80

        it "returns 0 MSI when all mutants resulted in errors" $ do
            let s = MAnalysisSummary (-1) 5 0 0 5 0
            summaryMsi s `shouldBe` 0

        it "calculates summaryCoveredMsi when coverage is present" $ do
            let s = MAnalysisSummary 10 10 2 8 0 0
            summaryCoveredMsi s `shouldBe` Just 80

        it "returns Nothing for summaryCoveredMsi when coverage is not present (-1)" $ do
            let s = MAnalysisSummary (-1) 10 2 8 0 0
            summaryCoveredMsi s `shouldBe` Nothing

    describe "Show instance" $ do
        it "produces formatted output containing MSI, Total mutants, and percentages" $ do
            let s = MAnalysisSummary (-1) 10 2 8 0 0
                out = show s
            out `shouldSatisfy` ("Total mutants:" `isInfixOf`)
            out `shouldSatisfy` ("Killed:" `isInfixOf`)
            out `shouldSatisfy` ("%" `isInfixOf`)
