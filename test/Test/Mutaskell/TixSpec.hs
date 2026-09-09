module Test.Mutaskell.TixSpec where

import Test.Hspec
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Mutaskell.Config (MuVar(..))
import Test.Mutaskell.Mutation (removeUncovered)
import Test.Mutaskell.TestAdapter (Mutant(..))
import Test.Mutaskell.Tix
    ( getUnCoveredPatchesFromIndex
    , buildTixIndex
    , insideSpan
    , indexSpans
    , parseTixIndex
    , removeRedundantSpans
    , spanIndexContains
    , toSpan
    )
import Trace.Hpc.Tix (TixModule (..))

mkMutant :: (Int, Int, Int, Int) -> Mutant
mkMutant coords = Mutant
  { _mutant = "src"
  , _mtype  = MutateValues
  , _mspan  = toSpan coords
  }

spec :: Spec
spec = describe "Test.Mutaskell.Tix" $ do
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

    it "preserves survivor order and duplicate spans" $
      let inner = toSpan (2, 2, 2, 8)
          touching = toSpan (4, 1, 4, 5)
          outer = toSpan (2, 1, 3, 10)
      in removeRedundantSpans [inner, touching, outer, outer]
          `shouldBe` [touching, outer, outer]

    it "keeps overlapping and boundary-touching spans" $
      let overlapA = toSpan (1, 1, 2, 5)
          overlapB = toSpan (2, 1, 3, 5)
          boundaryA = toSpan (4, 1, 4, 5)
          boundaryB = toSpan (4, 5, 5, 5)
      in removeRedundantSpans [overlapA, overlapB, boundaryA, boundaryB]
          `shouldBe` [overlapA, overlapB, boundaryA, boundaryB]

    it "agrees with pairwise containment on mixed span shapes" $
      let spans = map toSpan
            [ (1, 1, 1, 4)
            , (1, 1, 1, 4)
            , (1, 2, 1, 3)
            , (2, 1, 3, 5)
            , (3, 1, 4, 5)
            , (5, 1, 5, 5)
            , (5, 5, 6, 1)
            ]
          expected = filter (\sp -> not $ any (containsOther sp) spans) spans
          containsOther sp other = sp /= other && insideSpan sp other
      in removeRedundantSpans spans `shouldBe` expected

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

  describe "parsed coverage index" $ do
    it "answers containment queries with HPC's inclusive boundaries" $
      let indexed = indexSpans [toSpan (2, 1, 4, 10)]
          inside = toSpan (2, 1, 4, 10)
          boundary = toSpan (3, 1, 4, 10)
          overlapping = toSpan (4, 10, 5, 1)
      in map (spanIndexContains indexed) [inside, boundary, overlapping]
          `shouldBe` [True, True, False]

    it "answers an unmatched module query from a parsed tix snapshot" $
      withSystemTempDirectory "mutaskell-tix" $ \root -> do
        let path = root </> "coverage.tix"
        writeFile path "Tix []"
        index <- parseTixIndex path
        getUnCoveredPatchesFromIndex index "Missing"
          `shouldReturn` Right Nothing

    it "does not gate an ambiguous unqualified module name" $ do
      let index = buildTixIndex
            [ TixModule "one/Shared" 1 1 []
            , TixModule "two/Shared" 2 1 []
            ]
      getUnCoveredPatchesFromIndex index "Shared"
        `shouldReturn` Right Nothing
