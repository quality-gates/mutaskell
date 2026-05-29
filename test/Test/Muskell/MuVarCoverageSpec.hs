{-# LANGUAGE QuasiQuotes #-}

module Test.Muskell.MuVarCoverageSpec where

import Here
import Test.Hspec
import Test.Muskell.Config (defaultConfig, muOp)
import Test.Muskell.Mutation
import qualified Test.Muskell.MutationSpec.Helpers as H

-- | Minimal source snippets that each trigger a specific mutator.

patternSrc :: String
patternSrc = [e|
module M where
f [] = 0
f (_:xs) = 1
|]

literalSrc :: String
literalSrc = [e|
module M where
f = 42
|]

functionSrc :: String
functionSrc = [e|
module M where
f x = x + 1
|]

ifElseSrc :: String
ifElseSrc = [e|
module M where
f x = if x > 0 then 1 else 0
|]

guardSrc :: String
guardSrc = [e|
module M where
f x | x > 0 = 1
    | otherwise = 0
|]

notSrc :: String
notSrc = [e|
module M where
f x = not x
|]

negSrc :: String
negSrc = [e|
module M where
f x = negate x
|]

caseAltSrc :: String
caseAltSrc = [e|
module M where
f mx = case mx of { Just a -> a ; Nothing -> 0 }
|]

caseDefaultSrc :: String
caseDefaultSrc = [e|
module M where
f mx = case mx of { Just a -> a ; _ -> 0 }
|]

stmtSrc :: String
stmtSrc = [e|
module M where
import System.IO
f = do
  let x = 1
  print x
  return x
|]

letSrc :: String
letSrc = [e|
module M where
f = let x = 1; y = 2 in x + y
|]

whereSrc :: String
whereSrc = [e|
module M where
f = x + y
  where
    x = 1
    y = 2
|]

selfAssignSrc :: String
selfAssignSrc = [e|
module M where
f = let x = x in x
|]

negLitSrc :: String
negLitSrc = [e|
module M where
f = 42
|]

stringLitSrc :: String
stringLitSrc = [e|
module M where
f s = s == "hello"
|]

boolOpSrc :: String
boolOpSrc = [e|
module M where
f a b = a && b
|]

flipMaybeSrc :: String
flipMaybeSrc = [e|
module M where
f x = Just x
|]

flipEitherSrc :: String
flipEitherSrc = [e|
module M where
f x = Right x
|]

forkIOSrc :: String
forkIOSrc = [e|
module M where
f action = forkIO action
|]

bracketSrc :: String
bracketSrc = [e|
module M where
f = bracket open close action
|]

errorGuardSrc :: String
errorGuardSrc = [e|
module M where
f = catch action handler
|]

mutableArgSrc :: String
mutableArgSrc = [e|
module M where
f ref = readIORef ref
|]

zeroReturnSrc :: String
zeroReturnSrc = [e|
module M where
f :: Int -> Bool
f x = x > 0
|]

listLiteralSrc :: String
listLiteralSrc = [e|
module M where
f = [1, 2, 3 :: Int]
|]

bindToSeqSrc :: String
bindToSeqSrc = [e|
module M where
import System.IO
f h = do
  x <- hGetLine h
  return x
|]

patConSrc :: String
patConSrc = [e|
module M where
f (Just x) = x
f Nothing  = 0
|]

appendStripSrc :: String
appendStripSrc = [e|
module M where
f xs ys = xs ++ ys
|]

flipArgsSrc :: String
flipArgsSrc = [e|
module M where
f x y = compare x y
|]

seqStripSrc :: String
seqStripSrc = [e|
module M where
f x y = seq x y
|]

tupleSwapSrc :: String
tupleSwapSrc = [e|
module M where
f x = (x, x)
|]

orderingLitSrc :: String
orderingLitSrc = [e|
module M where
f = GT
|]

spec :: Spec
spec = describe "MuVar coverage" $ do
  it "pattern-match produces at least one mutant" $ do
    ast <- H.ast patternSrc
    selectFnMatches ast `shouldSatisfy` (not . null)
  it "literal-values produces at least one mutant" $ do
    ast <- H.ast literalSrc
    selectLiteralOps ast `shouldSatisfy` (not . null)
  it "functions produces at least one mutant" $ do
    ast <- H.ast functionSrc
    selectFunctionOps (muOp defaultConfig) ast `shouldSatisfy` (not . null)
  it "negate-if-else produces at least one mutant" $ do
    ast <- H.ast ifElseSrc
    selectIfElseBoolNegOps ast `shouldSatisfy` (not . null)
  it "negate-guards produces at least one mutant" $ do
    ast <- H.ast guardSrc
    selectGuardedBoolNegOps ast `shouldSatisfy` (not . null)
  it "remove-not produces at least one mutant" $ do
    ast <- H.ast notSrc
    selectRemoveNotOps ast `shouldSatisfy` (not . null)
  it "remove-negation produces at least one mutant" $ do
    ast <- H.ast negSrc
    selectRemoveNegationOps ast `shouldSatisfy` (not . null)
  it "case-alt-remove produces at least one mutant" $ do
    ast <- H.ast caseAltSrc
    selectCaseAltRemoveOps ast `shouldSatisfy` (not . null)
  it "case-default-remove produces at least one mutant" $ do
    ast <- H.ast caseDefaultSrc
    selectCaseDefaultRemoveOps ast `shouldSatisfy` (not . null)
  it "remove-stmt produces at least one mutant" $ do
    ast <- H.ast stmtSrc
    selectRemoveStmtOps ast `shouldSatisfy` (not . null)
  it "remove-let-binding produces at least one mutant" $ do
    ast <- H.ast letSrc
    selectRemoveLetBindingOps ast `shouldSatisfy` (not . null)
  it "remove-where-binding produces at least one mutant" $ do
    ast <- H.ast whereSrc
    selectRemoveWhereBindingOps ast `shouldSatisfy` (not . null)
  it "remove-self-assign produces at least one mutant" $ do
    ast <- H.ast selfAssignSrc
    selectRemoveSelfAssignOps ast `shouldSatisfy` (not . null)
  it "negate-literal produces at least one mutant" $ do
    ast <- H.ast negLitSrc
    selectNegateLiteralOps ast `shouldSatisfy` (not . null)
  it "string-literal produces at least one mutant" $ do
    ast <- H.ast stringLitSrc
    selectStringLiteralOps ast `shouldSatisfy` (not . null)
  it "bool-operand produces at least one mutant" $ do
    ast <- H.ast boolOpSrc
    selectBoolOperandOps ast `shouldSatisfy` (not . null)
  it "flip-maybe produces at least one mutant" $ do
    ast <- H.ast flipMaybeSrc
    selectFlipMaybeOps ast `shouldSatisfy` (not . null)
  it "flip-either produces at least one mutant" $ do
    ast <- H.ast flipEitherSrc
    selectFlipEitherOps ast `shouldSatisfy` (not . null)
  it "remove-forkIO produces at least one mutant" $ do
    ast <- H.ast forkIOSrc
    selectRemoveForkIOOps ast `shouldSatisfy` (not . null)
  it "bracket-degenerate produces at least one mutant" $ do
    ast <- H.ast bracketSrc
    selectBracketDegenerateOps ast `shouldSatisfy` (not . null)
  it "error-guard produces at least one mutant" $ do
    ast <- H.ast errorGuardSrc
    selectErrorGuardOps ast `shouldSatisfy` (not . null)
  it "replace-mutable-arg produces at least one mutant" $ do
    ast <- H.ast mutableArgSrc
    selectReplaceMutableArgOps ast `shouldSatisfy` (not . null)
  it "zero-return produces at least one mutant" $ do
    ast <- H.ast zeroReturnSrc
    selectZeroReturnOps ast `shouldSatisfy` (not . null)
  it "list-literal produces at least one mutant" $ do
    ast <- H.ast listLiteralSrc
    selectExplicitListOps ast `shouldSatisfy` (not . null)
  it "bind-to-sequence produces at least one mutant" $ do
    ast <- H.ast bindToSeqSrc
    selectBindToSequenceOps ast `shouldSatisfy` (not . null)
  it "pattern-constructor produces at least one mutant" $ do
    ast <- H.ast patConSrc
    selectPatternConstructorFlipOps ast `shouldSatisfy` (not . null)
  it "append-strip produces at least one mutant" $ do
    ast <- H.ast appendStripSrc
    selectAppendStripOps ast `shouldSatisfy` (not . null)
  it "flip-args produces at least one mutant" $ do
    ast <- H.ast flipArgsSrc
    selectFlipArgsOps ast `shouldSatisfy` (not . null)
  it "seq-strip produces at least one mutant" $ do
    ast <- H.ast seqStripSrc
    selectSeqStripOps ast `shouldSatisfy` (not . null)
  it "tuple-swap produces at least one mutant" $ do
    ast <- H.ast tupleSwapSrc
    selectTupleSwapOps ast `shouldSatisfy` (not . null)
  it "ordering-literal produces at least one mutant" $ do
    ast <- H.ast orderingLitSrc
    selectOrderingLitOps ast `shouldSatisfy` (not . null)
