{-# LANGUAGE QuasiQuotes #-}

module Test.Mutaskell.MutationSpec where

import Control.Monad (forM_)
import Data.List (isInfixOf, nubBy)
import Here
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Language.Haskell.GHC.ExactPrint (exactPrint)
import Test.Mutaskell.Config (MuVar (..), defaultConfig, maxNumMutants)
import Test.Mutaskell.MuOp (mkMpMuOp)
import Test.Mutaskell.Mutation
import Test.Mutaskell.Tix (toSpan)
import Test.Mutaskell.TestAdapter (toMutant)
import Test.Mutaskell.Utils.Common (apTh)
import Test.Mutaskell.TestAdapter (Mutant (..))
import Test.Mutaskell.Utils.Syb (once, relevantOps)
import qualified Test.Mutaskell.MutationSpec.Helpers as H

main :: IO ()
main = hspec spec

mutantWith :: String -> Mutant
mutantWith src = Mutant src MutateValues (toSpan (1, 1, 1, 2))

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

        -- Regression: the branch swap used to reuse each branch's original
        -- entry delta verbatim, dropping the @else@ keyword and producing
        -- source that never compiled (silently counted as skipped).
        it "emits a complete, branch-swapped if expression" $ do
            let text =
                    [e|
module Prop where

myFn x = if x == 1 then True else False
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            map (unwords . words . _mutant) mutants
                `shouldSatisfy` any ("then False else True" `isInfixOf`)

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

        -- Regression: the negated guard was emitted as @not x == 1@ (parsed
        -- @(not x) == 1@), a precedence error that never compiled.  It must be
        -- parenthesised as @not (x == 1)@.
        it "parenthesises the negated guard expression" $ do
            let text =
                    [e|
module Prop where

myFn x | x == 1 = True
myFn _ | otherwise = False
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            map (unwords . words . _mutant) mutants
                `shouldSatisfy` any ("not (x == 1)" `isInfixOf`)

    describe "selectFlipMaybeOps" $ do
        it "returns muops for a module with Just/Nothing" $ do
            let text =
                    [e|
module Prop where
import Data.Maybe (isNothing)

myFn x = isNothing (Just x)
|]
            ast <- H.ast text
            let ops = selectFlipMaybeOps ast
            ops `shouldSatisfy` (not . null)
            ops `shouldSatisfy` all (("==>" `isInfixOf`) . show)

        -- Regression: @Nothing@ was flipped to a bare @Just undefined@
        -- application.  In a function-application context such as
        -- @isNothing Nothing@, exactPrint then produced
        -- @isNothing Just undefined@, which parses as
        -- @(isNothing Just) undefined@ and never typechecks.  The injected
        -- application must be parenthesised.
        it "parenthesises Just undefined when replacing Nothing in an application" $ do
            let text =
                    [e|
module Prop where
import Data.Maybe (isNothing)

myFn x = isNothing Nothing
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let normSrcs = map (unwords . words . _mutant) mutants
            normSrcs `shouldSatisfy` any ("isNothing (Just undefined)" `isInfixOf`)
            normSrcs `shouldSatisfy` all (not . ("isNothing Just undefined" `isInfixOf`))

        it "parenthesises Just undefined when replacing Nothing in an operator argument" $ do
            let text =
                    [e|
module Prop where
import Data.Maybe (fromMaybe, isJust)

myFn k m = (fromMaybe Nothing m == Nothing) || isJust Nothing
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let normSrcs = map (unwords . words . _mutant) mutants
            normSrcs `shouldSatisfy` any ("== (Just undefined)" `isInfixOf`)
            normSrcs `shouldSatisfy` any ("isJust (Just undefined)" `isInfixOf`)
            normSrcs `shouldSatisfy` any ("fromMaybe (Just undefined) m" `isInfixOf`)
            normSrcs `shouldSatisfy` all (not . ("== Just undefined" `isInfixOf`))
            normSrcs `shouldSatisfy` all (not . ("isJust Just undefined" `isInfixOf`))
            normSrcs `shouldSatisfy` all (not . ("fromMaybe Just undefined" `isInfixOf`))

        it "still flips Just x to Nothing" $ do
            let text =
                    [e|
module Prop where

myFn x = Just x
|]
            ast <- H.ast text
            let ops = selectFlipMaybeOps ast
            ops `shouldSatisfy` (not . null)
            let srcs = map (unwords . words . exactPrint) $
                    concat [once (mkMpMuOp op) ast | op <- ops]
            srcs `shouldSatisfy` any ("myFn x = Nothing" `isInfixOf`)

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

    describe "selectNegateLiteralOps" $ do
        it "returns muops for a module with a positive numeric literal" $ do
            let text =
                    [e|
module Prop where
f x = add 5 x
|]
            ast <- H.ast text
            let ops = selectNegateLiteralOps ast
            ops `shouldSatisfy` (not . null)
            ops `shouldSatisfy` all (("==>" `isInfixOf`) . show)

        -- Regression: a positive literal was replaced with a bare
        -- @negate x@ application.  In a function-application context such as
        -- @add 5 x@, exactPrint then produced @add negate 5 x@, which parses
        -- as @(add negate) 5 x@ and never typechecks.  The injected
        -- application must be parenthesised.
        it "parenthesises negate when replacing a literal in a function application" $ do
            let text =
                    [e|
module Prop where
f x = add 5 x
|]
            ast <- H.ast text
            let ops = selectNegateLiteralOps ast
            ops `shouldSatisfy` (not . null)
            let srcs = map (unwords . words . exactPrint) $
                    concat [once (mkMpMuOp op) ast | op <- ops]
            srcs `shouldSatisfy` any ("add (negate 5) x" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("add negate 5" `isInfixOf`))

        it "parenthesises negate when replacing a literal in a constructor application" $ do
            let text =
                    [e|
module Prop where
g = Left 5
|]
            ast <- H.ast text
            let ops = selectNegateLiteralOps ast
            ops `shouldSatisfy` (not . null)
            let srcs = map (unwords . words . exactPrint) $
                    concat [once (mkMpMuOp op) ast | op <- ops]
            srcs `shouldSatisfy` any ("Left (negate 5)" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("Left negate 5" `isInfixOf`))

        it "parenthesises negate when replacing a fractional literal" $ do
            let text =
                    [e|
module Prop where
h x = foo 3.14 x
|]
            ast <- H.ast text
            let ops = selectNegateLiteralOps ast
            ops `shouldSatisfy` (not . null)
            let srcs = map (unwords . words . exactPrint) $
                    concat [once (mkMpMuOp op) ast | op <- ops]
            srcs `shouldSatisfy` any ("foo (negate" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("foo negate" `isInfixOf`))

    describe "selectErrorGuardOps" $ do
        -- Regression: handle handler action was replaced with handler
        -- (type e -> IO a) instead of action (type IO a).
        it "replaces handle handler action with action, not the handler" $ do
            let text =
                    [e|
module M where
f action = handle handler action
|]
            ast <- H.ast text
            let ops = selectErrorGuardOps ast
            ops `shouldSatisfy` (not . null)
            let srcs = map (unwords . words . exactPrint) $
                    concat [once (mkMpMuOp op) ast | op <- ops]
            srcs `shouldSatisfy` any ("f action = action" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("f action = handler" `isInfixOf`))

        it "replaces catch action handler with action" $ do
            let text =
                    [e|
module M where
f action = catch action handler
|]
            ast <- H.ast text
            let ops = selectErrorGuardOps ast
            ops `shouldSatisfy` (not . null)
            let srcs = map (unwords . words . exactPrint) $
                    concat [once (mkMpMuOp op) ast | op <- ops]
            srcs `shouldSatisfy` any ("f action = action" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("f action = handler" `isInfixOf`))

        it "replaces try action with return (Right action)" $ do
            let text =
                    [e|
module M where
f action = try action
|]
            ast <- H.ast text
            let ops = selectErrorGuardOps ast
            ops `shouldSatisfy` (not . null)
            let srcs = map (unwords . words . exactPrint) $
                    concat [once (mkMpMuOp op) ast | op <- ops]
            srcs `shouldSatisfy` any ("return (Right action)" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("return Right action" `isInfixOf`))

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
        it "renders _ <- action without dropping <- or fusing into _action" $ do
            let text =
                    [e|
module Prop where
testAction = do
  result <- performComputation 123
  return result
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let srcs = map _mutant mutants
                normSrcs = map (unwords . words) srcs
            normSrcs `shouldSatisfy` any ("_ <- performComputation 123" `isInfixOf`)
            normSrcs `shouldSatisfy` all (not . ("_performComputation" `isInfixOf`))
            let wildMutants = [ m | (m, nm) <- zip srcs normSrcs, "_ <- performComputation 123" `isInfixOf` nm ]
            wildMutants `shouldSatisfy` (not . null)
            mapM_ H.ast wildMutants
        it "renders multiple binds correctly across functions with _ <- when mutated" $ do
            let text =
                    [e|
module Prop where
testAction1 = do
  a <- getA
  return a

testAction2 = do
  b <- getB
  return b
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let srcs = map _mutant mutants
                normSrcs = map (unwords . words) srcs
            normSrcs `shouldSatisfy` any ("_ <- getA" `isInfixOf`)
            normSrcs `shouldSatisfy` any ("_ <- getB" `isInfixOf`)
            normSrcs `shouldSatisfy` all (not . ("_getA" `isInfixOf`))
            normSrcs `shouldSatisfy` all (not . ("_getB" `isInfixOf`))
            let wildMutants = [ m | (m, nm) <- zip srcs normSrcs, "_ <- getA" `isInfixOf` nm || "_ <- getB" `isInfixOf` nm ]
            wildMutants `shouldSatisfy` (not . null)
            mapM_ H.ast wildMutants
        it "renders both binds as _ <- when each is mutated in a single do block" $ do
            let text =
                    [e|
module Prop where
testAction = do
  a <- getA
  b <- getB
  return (a + b)
|]
            ast <- H.ast text
            let ops = selectBindToSequenceOps ast
            length ops `shouldBe` 2
            let rendered = [ exactPrint mutatedAst
                           | op <- ops
                           , mutatedAst <- once (mkMpMuOp op) ast
                           ]
            let renderedNorm = map (unwords . words) rendered
            renderedNorm `shouldSatisfy` any ("_ <- getA" `isInfixOf`)
            renderedNorm `shouldSatisfy` any ("_ <- getB" `isInfixOf`)
            renderedNorm `shouldSatisfy` all (not . ("_getA" `isInfixOf`))
            renderedNorm `shouldSatisfy` all (not . ("_getB" `isInfixOf`))
            mapM_ H.ast rendered

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
        it "renders (Just _) when flipping Nothing in a function head" $ do
            let text =
                    [e|
module Prop where
f Nothing = 0
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let srcs = map (unwords . words . _mutant) mutants
            srcs `shouldSatisfy` any ("f (Just _) = 0" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("Just_" `isInfixOf`))
        it "renders (Just _) when flipping Nothing in a multi-clause function" $ do
            let text =
                    [e|
module Prop where
f Nothing = 0
f (Just x) = x
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let srcs = map (unwords . words . _mutant) mutants
            srcs `shouldSatisfy` any ("f (Just _) = 0" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("Just_" `isInfixOf`))
        it "renders (Just _) when flipping Nothing in a case expression" $ do
            let text =
                    [e|
module Prop where
g x = case x of
    Nothing -> 0
    Just v  -> v
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let srcs = map (unwords . words . _mutant) mutants
            srcs `shouldSatisfy` any ("(Just _) -> 0" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("Just_" `isInfixOf`))
        it "renders (Just _) without duplicate parens when flipping parenthesised (Nothing)" $ do
            let text =
                    [e|
module Prop where
f (Nothing) = 0
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let srcs = map (unwords . words . _mutant) mutants
            srcs `shouldSatisfy` any ("f (Just _) = 0" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("((Just _))" `isInfixOf`))
            srcs `shouldSatisfy` all (not . ("Just_" `isInfixOf`))
        it "renders (Nothing) when flipping (Just x) in a function head" $ do
            let text =
                    [e|
module Prop where
f (Just x) = x
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let srcs = map (unwords . words . _mutant) mutants
            srcs `shouldSatisfy` any ("f (Nothing) = x" `isInfixOf`)
        it "renders (Right e) when flipping (Left e) in a function head" $ do
            let text =
                    [e|
module Prop where
f (Left e) = 0
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let srcs = map (unwords . words . _mutant) mutants
            srcs `shouldSatisfy` any ("f (Right e) = 0" `isInfixOf`)

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

    describe "default neighbour-swap groups" $ do
        -- Regression: identifier substitution built a fresh node and dropped
        -- the name's backquote adornment, so `x `div` y` was emitted as the
        -- ill-typed `x quot y` and silently skipped.
        it "preserves backquotes when substituting an infix identifier" $ do
            let text =
                    [e|
module Prop where

halve x = x `div` 2
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let srcs = map (unwords . words . _mutant) mutants
            srcs `shouldSatisfy` any ("x `quot` 2" `isInfixOf`)
            srcs `shouldSatisfy` all (not . ("x quot 2" `isInfixOf`))

        it "swaps idiomatic neighbour functions via the default config" $ do
            let text =
                    [e|
module Prop where

f xs = all even xs
g xs = take 3 xs
|]
            Right mutants <- genMutantsForSrc defaultConfig text
            let srcs = map (unwords . words . _mutant) mutants
            srcs `shouldSatisfy` any ("any even" `isInfixOf`)
            srcs `shouldSatisfy` any ("drop 3" `isInfixOf`)

    describe "adjacentSwaps" $ do
        it "produces n-1 adjacent-pair swaps, not the n! permutations" $
            adjacentSwaps [1 :: Int, 2, 3] `shouldBe` [[2, 1, 3], [1, 3, 2]]
        it "is empty for a singleton (nothing to reorder)" $
            adjacentSwaps [1 :: Int] `shouldBe` []

    describe "selector metadata" $ do
        it "excludes annotated test declarations from generated source" $ do
            let text =
                    [e|
module M where
{-# ANN annotated "Test" #-}
annotated = 1
production = 2
|]
            ast <- H.ast text
            getAnnotatedTests ast `shouldBe` ["annotated"]
            let (testDecls, sourceDecls) = splitAnnotations ast
            filter (not . null) (map functionName testDecls)
                `shouldBe` ["annotated"]
            map functionName sourceDecls `shouldSatisfy` elem "production"
            Right mutants <- genMutantsForSrc defaultConfig text
            let sources = map (unwords . words . _mutant) mutants
            sources `shouldSatisfy` all ("annotated = 1" `isInfixOf`)
            sources `shouldSatisfy` any (not . ("production = 2" `isInfixOf`))

        it "excludes naming-convention test declarations when there are no annotations" $ do
            let text =
                    [e|
module M where
prop_generated = 1
production = 2
|]
            ast <- H.ast text
            getAnnotatedTests ast `shouldBe` ["prop_generated"]
            let (testDecls, sourceDecls) = splitAnnotations ast
            map functionName testDecls `shouldBe` ["prop_generated"]
            map functionName sourceDecls `shouldSatisfy` elem "production"
            Right mutants <- genMutantsForSrc defaultConfig text
            let sources = map (unwords . words . _mutant) mutants
            sources `shouldSatisfy` all ("prop_generated = 1" `isInfixOf`)
            sources `shouldSatisfy` any (not . ("production = 2" `isInfixOf`))

        it "applies a multi-name signature to every declared function" $ do
            let text =
                    [e|
module M where
f, g :: Int -> Bool
f x = x > 0
g x = x > 0
|]
            ast <- H.ast text
            let renderOps ops =
                    map (unwords . words . exactPrint) $
                        concat [once (mkMpMuOp op) ast | op <- ops]
                sources = renderOps (selectZeroReturnOps ast)
            sources `shouldSatisfy` any ("f x = False" `isInfixOf`)
            sources `shouldSatisfy` any ("g x = False" `isInfixOf`)

        it "keeps the first signature for a repeated function name" $ do
            let text =
                    [e|
module M where
f :: Int -> Int
f :: Int -> Bool
f x = x > 0
|]
            ast <- H.ast text
            let sources =
                    map (unwords . words . exactPrint) $
                        concat [once (mkMpMuOp op) ast
                               | op <- selectZeroReturnOps ast]
            sources `shouldSatisfy` any ("f x = 0" `isInfixOf`)
            sources `shouldSatisfy` all (not . ("f x = False" `isInfixOf`))

        it "rejects an identity operation before sampling but keeps a real change" $ do
            let sameText =
                    [e|
module M where
f x = if x > 0 then 1 else 1
|]
                changedText =
                    [e|
module M where
f x = if x > 0 then 1 else 0
|]
            sameAst <- H.ast sameText
            changedAst <- H.ast changedText
            let classify ast =
                    relevantOps ast
                        [ (MutateNegateIfElse, op)
                        | op <- selectIfElseBoolNegOps ast
                        ]
            length (classify sameAst) `shouldBe` 0
            classify changedAst `shouldSatisfy` (not . null)

    describe "genSampledMutants" $ do
        it "returns a non-empty, rendered mutant set for a module with mutables" $ do
            let text =
                    [e|
module Prop where

myFn x = if x == 1 then x + 2 else x - 3
|]
            ast <- H.ast text
            ms <- genSampledMutants defaultConfig ast
            ms `shouldSatisfy` (not . null)
            map _mutant ms `shouldSatisfy` all (not . null)

        it "never exceeds the configured maxNumMutants cap" $ do
            let text =
                    [e|
module Prop where

a = 10
b = 20
c = 30
d = 40
e = 50
|]
            ast <- H.ast text
            ms <- genSampledMutants (defaultConfig { maxNumMutants = 3 }) ast
            length ms `shouldSatisfy` (<= 3)

        it "returns nothing for a zero or negative cap" $ do
            ast <- H.ast "module M where\nf x = x + 1\n"
            ms <- genSampledMutants (defaultConfig { maxNumMutants = 0 }) ast
            ms `shouldBe` []
            ms' <- genSampledMutants (defaultConfig { maxNumMutants = -1 }) ast
            ms' `shouldBe` []

        it "keeps materialization bounded by the cap as the module grows" $ do
            let dense n = unlines
                    ("module Dense where" : ["f" ++ show i ++ " x = x + 1" | i <- [1..n]])
            forM_ [20, 60, 120 :: Int] $ \n -> do
                ast <- H.ast (dense n)
                sampled <- genSampledMutants (defaultConfig { maxNumMutants = 2 }) ast
                -- The cap is spent on operators before rendering, so the
                -- rendered count stays at the cap no matter how many
                -- candidates the module would offer.
                length sampled `shouldSatisfy` (<= 2)
                sampled `shouldSatisfy` (not . null)
                let exact = genMutantsFromAST (defaultConfig { maxNumMutants = 2 }) ast
                length exact `shouldSatisfy` (> 2)

        it "may underfill the cap when selected operators collapse to duplicates" $ do
            ast <- H.ast "module M where\nf x = x + 1\n"
            ms <- genSampledMutants (defaultConfig { maxNumMutants = 100 }) ast
            -- Every operator is selected under a cap above the candidate
            -- count, and same-site variants collapse to one mutant each, so
            -- the rendered set is smaller than the cap.
            ms `shouldSatisfy` (not . null)
            length ms `shouldSatisfy` (< 100)

    describe "dedupRenderedSource" $ do
        it "keeps the first value for each rendered source" $
            map _mutant (nubRendered [mutantWith "a", mutantWith "b", mutantWith "a"])
                `shouldBe` ["a", "b"]

        it "retains every distinct source when hash keys collide" $
            -- Every candidate shares one hash key, so the index must verify
            -- the rendered sources themselves.
            dedupRenderedSource id (const 0) ["b", "a", "b", "c"] `shouldBe` ["b", "a", "c"]

        it "drops nothing for distinct hash keys" $
            dedupRenderedSource id (length :: String -> Int) ["a", "b", "c", "a"]
                `shouldBe` ["a", "b", "c"]

    describe "nubOpSites" $ do
        it "keeps the first candidate for each mutator and span" $
            let sites = [(MutateValues, toSpan (1, 1, 1, 2), ())
                        ,(MutateValues, toSpan (1, 1, 1, 2), ())
                        ,(MutateValues, toSpan (2, 1, 2, 2), ())
                        ,(MutateFunctions, toSpan (1, 1, 1, 2), ())]
            in nubOpSites sites `shouldBe`
                  [(MutateValues, toSpan (1, 1, 1, 2), ())
                  ,(MutateValues, toSpan (2, 1, 2, 2), ())
                  ,(MutateFunctions, toSpan (1, 1, 1, 2), ())]

        it "matches the equivalent pairwise dedup on a mixed fixture" $ do
            let text =
                    [e|
module M where
f x = x + 1
g y = if y > 0 then y else y
h b = b && (not b)
|]
            ast <- H.ast text
            let (origStr, ops) = prepareSelectorInputs defaultConfig [] ast
                -- The straightforward pairwise definition of the same
                -- semantics, used as an independent oracle for the indexed
                -- deduplication.
                reference =
                    nubBy (\a b -> _mutant a == _mutant b) $
                        filter (\m -> _mutant m /= origStr) $
                            map (toMutant . apTh exactPrint) $
                                nubBy
                                    (\(v1, s1, _) (v2, s2, _) -> v1 == v2 && s1 == s2)
                                    (mutatesN ops ast 1)
            genMutantsFromAST defaultConfig ast `shouldBe` reference

    describe "needsCabalMacros" $ do
        it "returns True for source containing MIN_VERSION_*" $ do
            needsCabalMacros "#if MIN_VERSION_base(4,16,0)" `shouldBe` True
        it "returns False for source with no MIN_VERSION_* guards" $ do
            needsCabalMacros "#if __GLASGOW_HASKELL__ >= 900" `shouldBe` False
        it "returns False for plain source" $ do
            needsCabalMacros "module Foo where\nfoo = 1" `shouldBe` False

    describe "getASTFromFile" $ do
        it "returns a clean Left for a CPP file using MIN_VERSION_* when no dist-newstyle exists" $ do
            withSystemTempDirectory "mutaskell-test" $ \tmpDir -> do
                let srcFile = tmpDir ++ "/MinVersion.hs"
                writeFile srcFile $ unlines
                    [ "{-# LANGUAGE CPP #-}"
                    , "module MinVersion where"
                    , "#if MIN_VERSION_base(4,16,0)"
                    , "foo :: Int"
                    , "foo = 1"
                    , "#else"
                    , "foo :: Int"
                    , "foo = 0"
                    , "#endif"
                    ]
                -- Run from tmpDir so discoverCabalMacros finds no dist-newstyle/
                result <- withCurrentDirectory tmpDir $ getASTFromFile srcFile
                case result of
                    Left msg -> msg `shouldSatisfy` ("MIN_VERSION_" `isInfixOf`)
                    Right _  -> expectationFailure
                                    "expected Left (CPP parse skipped) but got Right"

    describe "discoverCabalMacros" $
        it "scans the build tree once for repeated CPP parses in one project" $
            withSystemTempDirectory "mutaskell-test" $ \tmpDir -> do
                -- A fake build tree with a macros header, so the scan is real
                -- work that the cache can elide.
                createDirectoryIfMissing True (tmpDir </> "dist-newstyle/cache/build")
                writeFile (tmpDir </> "dist-newstyle/cache/build/cabal_macros.h") ""
                let cppFile i = tmpDir ++ "/Cpp" ++ show i ++ ".hs"
                forM_ [1, 2 :: Int] $ \i ->
                    writeFile (cppFile i) $ unlines
                        [ "{-# LANGUAGE CPP #-}"
                        , "module Cpp" ++ show i ++ " where"
                        , "#if __GLASGOW_HASKELL__ >= 900"
                        , "foo :: Int"
                        , "foo = 1"
                        , "#endif"
                        ]
                scansBefore <- readCabalMacroScans
                _ <- withCurrentDirectory tmpDir $
                    mapM (\i -> getASTFromFile (cppFile i) >> getASTFromFile (cppFile i)) [1, 2]
                -- Four CPP parses, one build-tree scan: the second project in
                -- the same process would scan again (different dist path).
                readCabalMacroScans `shouldReturn` scansBefore + 1
