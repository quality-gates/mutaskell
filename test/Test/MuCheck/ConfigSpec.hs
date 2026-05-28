module Test.MuCheck.ConfigSpec where

import Test.Hspec
import Test.MuCheck.Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "showMuVar / parseMuVar" $ do
        it "round-trips MutatePatternMatch" $
            parseMuVar (showMuVar MutatePatternMatch) `shouldBe` Just MutatePatternMatch

        it "round-trips MutateValues" $
            parseMuVar (showMuVar MutateValues) `shouldBe` Just MutateValues

        it "round-trips MutateFunctions" $
            parseMuVar (showMuVar MutateFunctions) `shouldBe` Just MutateFunctions

        it "round-trips MutateNegateIfElse" $
            parseMuVar (showMuVar MutateNegateIfElse) `shouldBe` Just MutateNegateIfElse

        it "round-trips MutateNegateGuards" $
            parseMuVar (showMuVar MutateNegateGuards) `shouldBe` Just MutateNegateGuards

        it "round-trips MutateOther remove-not (special case)" $
            parseMuVar (showMuVar (MutateOther "remove-not")) `shouldBe` Just (MutateOther "remove-not")

        it "round-trips MutateOther remove-negation (special case)" $
            parseMuVar (showMuVar (MutateOther "remove-negation")) `shouldBe` Just (MutateOther "remove-negation")

        it "round-trips MutateOther general case" $
            parseMuVar (showMuVar (MutateOther "case-alt-remove")) `shouldBe` Just (MutateOther "case-alt-remove")

        it "round-trips MutateOther empty string" $
            parseMuVar (showMuVar (MutateOther "")) `shouldBe` Just (MutateOther "")

        it "returns Nothing for unrecognised name" $
            parseMuVar "not-a-mutator" `shouldBe` Nothing

    describe "matchesMuVarPat" $ do
        it "exact match works" $
            matchesMuVarPat "functions" MutateFunctions `shouldBe` True

        it "exact non-match returns False" $
            matchesMuVarPat "functions" MutateValues `shouldBe` False

        it "trailing wildcard matches MutateOther variants" $
            matchesMuVarPat "other:*" (MutateOther "case-alt-remove") `shouldBe` True

        it "trailing wildcard does not match non-other variants" $
            matchesMuVarPat "other:*" MutateFunctions `shouldBe` False
