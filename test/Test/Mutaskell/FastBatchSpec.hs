module Test.Mutaskell.FastBatchSpec where

import Data.Maybe (isNothing)
import Test.Hspec
import Test.Mutaskell.AnalysisSummary (MAnalysisSummary (..))
import Test.Mutaskell.Config (defaultConfig, maxNumMutants)
import Test.Mutaskell.FastBatch (tryFastBatch)

spec :: Spec
spec = describe "tryFastBatch" $ do
    it "kills every boolean literal flip" $ do
        result <- tryFastBatch (maxNumMutants defaultConfig) boolSrc
        case result of
            Nothing -> expectationFailure "boolean module did not take the batch path"
            Just (summary, _, _) -> do
                _maErrors summary `shouldBe` 0
                _maSkipped summary `shouldBe` 0
                _maAlive summary `shouldBe` 0
                _maKilled summary `shouldBe` _maNumMutants summary
                _maNumMutants summary `shouldSatisfy` (>= 2)

    it "kills a flipped literal under not" $ do
        result <- tryFastBatch (maxNumMutants defaultConfig) notSrc
        case result of
            Nothing -> expectationFailure "not-expression did not take the batch path"
            Just (summary, _, _) -> do
                _maErrors summary `shouldBe` 0
                _maSkipped summary `shouldBe` 0
                _maAlive summary `shouldBe` 0
                _maKilled summary `shouldBe` _maNumMutants summary
                _maNumMutants summary `shouldSatisfy` (>= 1)

    it "leaves an or-expression alive when the flipped literal still passes" $ do
        result <- tryFastBatch (maxNumMutants defaultConfig) orSrc
        case result of
            Nothing -> expectationFailure "or-expression did not take the batch path"
            Just (summary, _, _) -> do
                _maErrors summary `shouldBe` 0
                _maSkipped summary `shouldBe` 0
                _maKilled summary `shouldSatisfy` (>= 1)
                _maAlive summary `shouldSatisfy` (>= 1)
                (_maKilled summary + _maAlive summary) `shouldBe` _maNumMutants summary

    it "refuses a module that is not only boolean flips" $ do
        result <- tryFastBatch (maxNumMutants defaultConfig) opSrc
        result `shouldSatisfy` isNothing

boolSrc :: String
boolSrc = unlines
    [ "module Bools where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "flagA = True"
    , "flagB = True"
    , "test_both = assertCheck (flagA && flagB)"
    ]

notSrc :: String
notSrc = unlines
    [ "module Nots where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "flag = False"
    , "test_flag = assertCheck (not flag)"
    ]

orSrc :: String
orSrc = unlines
    [ "module Ors where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "flagA = True"
    , "flagB = False"
    , "test_or = assertCheck (flagA || flagB)"
    ]

opSrc :: String
opSrc = unlines
    [ "module Ops where"
    , "import Test.Mutaskell.TestAdapter.AssertCheck"
    , "inc x = x + 1"
    , "test_inc = assertCheck (inc 1 == 2)"
    ]
