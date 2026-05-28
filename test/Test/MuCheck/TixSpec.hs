module Test.MuCheck.TixSpec where

import Test.Hspec
import Test.MuCheck.Config (MuVar(..))
import Test.MuCheck.Mutation (removeUncovered)
import Test.MuCheck.TestAdapter (Mutant(..))
import Test.MuCheck.Tix (removeRedundantSpans, toSpan)

mkMutant :: (Int, Int, Int, Int) -> Mutant
mkMutant coords = Mutant
  { _mutant = "src"
  , _mtype  = MutateValues
  , _mspan  = toSpan coords
  }

spec :: Spec
spec = describe "Test.MuCheck.Tix" $ do
  describe "removeRedundantSpans" $ do
    it "returns empty list unchanged" $
      removeRedundantSpans [] `shouldBe` []

    it "returns a single span unchanged" $
      let sp = toSpan (1, 1, 1, 10)
      in removeRedundantSpans [sp] `shouldBe` [sp]

    it "removes a span that is inside a larger span" $
      let inner = toSpan (2, 3, 2, 8)
          outer = toSpan (2, 1, 2, 10)
      in removeRedundantSpans [inner, outer] `shouldBe` [outer]

    it "keeps both spans when neither contains the other" $
      let sp1 = toSpan (1, 1, 1, 5)
          sp2 = toSpan (2, 1, 2, 5)
      in removeRedundantSpans [sp1, sp2] `shouldBe` [sp1, sp2]

  describe "removeUncovered" $ do
    it "returns all mutants when uncovered span list is empty" $
      let ms = [mkMutant (3, 1, 3, 10), mkMutant (5, 1, 5, 10)]
      in length (removeUncovered [] ms) `shouldBe` 2

    it "removes a mutant whose span falls inside an uncovered span" $
      let uncovered = [toSpan (3, 1, 3, 20)]
          m1 = mkMutant (3, 5, 3, 10)  -- inside uncovered
          m2 = mkMutant (5, 1, 5, 10)  -- outside uncovered
      in map _mspan (removeUncovered uncovered [m1, m2]) `shouldBe` [_mspan m2]

    it "keeps a mutant whose span is outside all uncovered spans" $
      let uncovered = [toSpan (10, 1, 15, 1)]
          m = mkMutant (3, 1, 3, 10)
      in removeUncovered uncovered [m] `shouldBe` [m]

    it "removes all mutants when they all fall inside uncovered spans" $
      let uncovered = [toSpan (1, 1, 20, 1)]
          ms = [mkMutant (3, 1, 3, 10), mkMutant (5, 1, 5, 10)]
      in removeUncovered uncovered ms `shouldBe` []
