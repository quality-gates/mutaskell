module Test.Mutaskell.Utils.CommonSpec (main, spec) where

import Data.List (isSubsequenceOf, nub, sort)
import qualified Data.Map.Strict as Map
import System.Random
import Test.Hspec
import Test.Mutaskell.Utils.Common (choose, coupling, remElt, replaceFst, sample, sampleF, spread, strip)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "replaceFst" $ do
        it "if given empty list dont do any thing" $ do
            replaceFst (1, 2) ([] :: [Int]) `shouldBe` ([] :: [Int])

        it "if given a list with out value dont do any thing" $ do
            replaceFst (1, 2) ([3] :: [Int]) `shouldBe` ([3] :: [Int])

        it "if given a list with value replaceFst" $ do
            replaceFst (1, 2) ([1] :: [Int]) `shouldBe` ([2] :: [Int])

    describe "choose" $ do
        it "if given empty return empty" $ do
            choose ([] :: [Int]) 10 `shouldBe` ([] :: [[Int]])
        it "if given zero return e.empty" $ do
            choose [1] 0 `shouldBe` [[]]
        it "if given list return subset with given size" $ do
            choose [1, 2, 3] 2 `shouldBe` [[1, 2], [1, 3], [2, 3]]

    describe "remElt" $ do
        it "must remove element at given index" $ do
            remElt 2 [1, 2, 3, 4] `shouldBe` [1, 2, 4]

    describe "sample" $ do
        it "must sample a given size subset" $ do
            let xs = [1, 2, 3, 4] :: [Int]
                got = sample (mkStdGen 1) 2 xs
            length got `shouldBe` 2
            sort got `shouldSatisfy` (`isSubsequenceOf` sort xs)

        it "returns empty list when n is negative" $ do
            sample (mkStdGen 42) (-1) [1 :: Int, 2, 3] `shouldBe` []
            sample (mkStdGen 42) (-5) [1 :: Int, 2, 3] `shouldBe` []

        it "returns empty list when n is negative and list is empty" $ do
            sample (mkStdGen 42) (-1) ([] :: [Int]) `shouldBe` []

        it "returns empty list when n is zero" $ do
            sample (mkStdGen 42) 0 [1 :: Int, 2, 3] `shouldBe` []
            sample (mkStdGen 42) 0 ([] :: [Int]) `shouldBe` []

        it "returns the input in order when n equals the input length" $ do
            sample (mkStdGen 1) 4 [1, 2, 3, 4 :: Int] `shouldBe` [1, 2, 3, 4]

        it "returns the input in order when n exceeds the input length" $ do
            sample (mkStdGen 1) 10 [1, 2, 3 :: Int] `shouldBe` [1, 2, 3]

        it "returns empty input unchanged when n exceeds length" $ do
            sample (mkStdGen 1) 3 ([] :: [Int]) `shouldBe` []

        it "samples positions without replacement" $ do
            let xs = [10, 20, 30, 40, 50, 60] :: [Int]
                got = sample (mkStdGen 99) 3 xs
            length got `shouldBe` 3
            nub got `shouldBe` got
            sort got `shouldSatisfy` (`isSubsequenceOf` sort xs)

        it "keeps equal values that come from distinct positions" $ do
            let xs = [1, 1, 2, 2] :: [Int]
                got = sample (mkStdGen 5) 3 xs
            length got `shouldBe` 3
            sort got `shouldSatisfy` (`isSubsequenceOf` sort xs)

        it "is reproducible for the same seed and input" $ do
            let xs = [1 .. 100] :: [Int]
            sample (mkStdGen 123) 17 xs `shouldBe` sample (mkStdGen 123) 17 xs

        it "selects each position with similar frequency" $ do
            let xs = [0 .. 9] :: [Int]
                n = 3
                trials = 2000
                counts = Map.fromListWith (+)
                    [ (x, 1 :: Int)
                    | seed <- [1 .. trials]
                    , x <- sample (mkStdGen seed) n xs
                    ]
                expected = fromIntegral trials * n `div` length xs
            Map.keys counts `shouldBe` xs
            Map.elems counts `shouldSatisfy` all (\c -> abs (c - expected) < expected `div` 3)

    describe "sampleF" $ do
        it "must sample a given fraction subset" $ do
            let xs = [1, 2, 3, 4] :: [Int]
                got = sampleF (mkStdGen 1) 0.5 xs
            length got `shouldBe` 2
            sort got `shouldSatisfy` (`isSubsequenceOf` sort xs)

        it "returns empty list when fraction is negative" $ do
            sampleF (mkStdGen 42) (-0.5) [1 :: Int, 2, 3] `shouldBe` []

        it "returns empty list when fraction is zero" $ do
            sampleF (mkStdGen 42) 0 [1 :: Int, 2, 3] `shouldBe` []

        it "rounds the quota from the fraction times the length" $ do
            let xs = [1 .. 5] :: [Int]
            length (sampleF (mkStdGen 7) 0.5 xs) `shouldBe` 2

        it "returns the input in order when the fraction is one" $ do
            sampleF (mkStdGen 1) 1 [1, 2, 3, 4 :: Int] `shouldBe` [1, 2, 3, 4]

        it "returns the input in order when the fraction exceeds one" $ do
            sampleF (mkStdGen 1) 2 [1, 2, 3 :: Int] `shouldBe` [1, 2, 3]

    describe "coupling" $ do
        it "must sample a given fraction subset" $ do
            coupling (+) [1, 2, 3] `shouldBe` [3, 4, 3, 5, 4, 5]

    describe "strip" $ do
        it "strips leading and trailing whitespace" $ do
            strip "  hello world  \n\t" `shouldBe` "hello world"
        it "returns empty string for whitespace-only input" $ do
            strip "   \t\n  " `shouldBe` ""

    describe "spread" $ do
        it "distributes the first element across the list" $ do
            spread (1 :: Int, ["a", "b", "c"]) `shouldBe` [(1, "a"), (1, "b"), (1, "c")]
