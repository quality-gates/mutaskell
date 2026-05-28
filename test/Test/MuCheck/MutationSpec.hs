{-# LANGUAGE QuasiQuotes #-}

module Test.MuCheck.MutationSpec where

import Data.List (isInfixOf)
import Here
import Test.Hspec
import Test.MuCheck.Mutation
import qualified Test.MuCheck.MutationSpec.Helpers as H

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
