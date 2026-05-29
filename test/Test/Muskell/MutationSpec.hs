{-# LANGUAGE QuasiQuotes #-}

module Test.Muskell.MutationSpec where

import Data.List (isInfixOf)
import Here
import Test.Hspec
import Test.Muskell.Mutation
import qualified Test.Muskell.MutationSpec.Helpers as H

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "selectLitOps" $ do
        it "returns integer literal muops for a module with a numeric literal" $ do
            let text =
                    [e|
module Prop where
import Test.QuickCheck

myFn x = if x == 1 then True else False
|]
            ast <- H.ast text
            let ops = selectLitOps ast
            ops `shouldSatisfy` (not . null)
            ops `shouldSatisfy` all (("==>" `isInfixOf`) . show)

    describe "selectBLitOps" $ do
        it "returns boolean literal muops for a module with boolean literals" $ do
            let text =
                    [e|
module Prop where
import Test.QuickCheck

myFn x = if x == 1 then True else False
|]
            ast <- H.ast text
            let ops = selectBLitOps ast
            ops `shouldSatisfy` (not . null)
            ops `shouldSatisfy` all (("==>" `isInfixOf`) . show)

    describe "selectIfElseBoolNegOps" $ do
        it "returns if-else muops for a module with an if expression" $ do
            let text =
                    [e|
module Prop where
import Test.QuickCheck

myFn x = if x == 1 then True else False
|]
            ast <- H.ast text
            let ops = selectIfElseBoolNegOps ast
            ops `shouldSatisfy` (not . null)
            ops `shouldSatisfy` all (("==>" `isInfixOf`) . show)

    describe "selectGuardedBoolNegOps" $ do
        it "returns guarded-boolean muops for a module with a guarded definition" $ do
            let text =
                    [e|
module Prop where

myFn x | x == 1 = True
myFn _ | otherwise = False
|]
            ast <- H.ast text
            let ops = selectGuardedBoolNegOps ast
            ops `shouldSatisfy` (not . null)
            ops `shouldSatisfy` all (("==>" `isInfixOf`) . show)

    describe "selectRemoveNotOps" $ do
        it "returns remove-not muops" $ do
            let text =
                    [e|
module Prop where
myFn x = not x
|]
            ast <- H.ast text
            selectRemoveNotOps ast `shouldSatisfy` (not . null)

    describe "selectRemoveNegationOps" $ do
        it "returns remove-negation muops" $ do
            let text =
                    [e|
module Prop where
myFn x = negate x
|]
            ast <- H.ast text
            selectRemoveNegationOps ast `shouldSatisfy` (not . null)

    describe "selectFnMatches" $ do
        it "returns function-match muops for a multi-clause function" $ do
            let text =
                    [e|
module Prop where
import Test.QuickCheck

myFn [] = 0
myFn (x:xs) = 1 + myFn xs
|]
            ast <- H.ast text
            let ops = selectFnMatches ast
            ops `shouldSatisfy` (not . null)
            ops `shouldSatisfy` all (("==>" `isInfixOf`) . show)

    describe "selectExplicitListOps" $ do
        it "returns muops for a non-empty explicit list literal" $ do
            let text =
                    [e|
module Prop where
myFn = [1, 2, 3 :: Int]
|]
            ast <- H.ast text
            selectExplicitListOps ast `shouldSatisfy` (not . null)

    describe "selectBindToSequenceOps" $ do
        it "returns muops for a do-block with a named bind" $ do
            let text =
                    [e|
module Prop where
import System.IO
myFn h = do
    x <- hGetLine h
    return x
|]
            ast <- H.ast text
            selectBindToSequenceOps ast `shouldSatisfy` (not . null)

    describe "selectPatternConstructorFlipOps" $ do
        it "returns muops for a function with a Just pattern" $ do
            let text =
                    [e|
module Prop where
myFn (Just x) = x
myFn Nothing  = 0
|]
            ast <- H.ast text
            selectPatternConstructorFlipOps ast `shouldSatisfy` (not . null)
        it "returns muops for a function with Left/Right patterns" $ do
            let text =
                    [e|
module Prop where
myFn (Left  e) = 0
myFn (Right v) = v
|]
            ast <- H.ast text
            selectPatternConstructorFlipOps ast `shouldSatisfy` (not . null)

    describe "selectAppendStripOps" $ do
        it "returns muops for a ++ expression" $ do
            let text =
                    [e|
module Prop where
myFn xs ys = xs ++ ys
|]
            ast <- H.ast text
            selectAppendStripOps ast `shouldSatisfy` (not . null)

    describe "selectFlipArgsOps" $ do
        it "returns muops for a known flippable binary function call" $ do
            let text =
                    [e|
module Prop where
myFn x y = compare x y
|]
            ast <- H.ast text
            selectFlipArgsOps ast `shouldSatisfy` (not . null)

    describe "selectSeqStripOps" $ do
        it "returns muops for a seq application" $ do
            let text =
                    [e|
module Prop where
myFn x y = seq x y
|]
            ast <- H.ast text
            selectSeqStripOps ast `shouldSatisfy` (not . null)

    describe "selectTupleSwapOps" $ do
        it "returns muops for a pair expression" $ do
            let text =
                    [e|
module Prop where
myFn x = (x, x)
|]
            ast <- H.ast text
            selectTupleSwapOps ast `shouldSatisfy` (not . null)

    describe "selectOrderingLitOps" $ do
        it "returns muops for a GT literal" $ do
            let text =
                    [e|
module Prop where
myFn = GT
|]
            ast <- H.ast text
            selectOrderingLitOps ast `shouldSatisfy` (not . null)
