{-# LANGUAGE QuasiQuotes #-}

module Test.MuCheck.MuVarCoverageSpec where

import Here
import Test.Hspec
import Test.MuCheck.Config (defaultConfig, muOp)
import Test.MuCheck.Mutation
import qualified Test.MuCheck.MutationSpec.Helpers as H

-- | Minimal source snippets that each trigger a specific mutator.
-- Each must be a well-formed Haskell module parseable by haskell-src-exts.

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

spec :: Spec
spec = describe "MuVar coverage" $ do
  it "pattern-match produces at least one mutant" $
    selectFnMatches (H.ast patternSrc) `shouldSatisfy` (not . null)
  it "literal-values produces at least one mutant" $
    selectLiteralOps (H.ast literalSrc) `shouldSatisfy` (not . null)
  it "functions produces at least one mutant" $
    selectFunctionOps (muOp defaultConfig) (H.ast functionSrc) `shouldSatisfy` (not . null)
  it "negate-if-else produces at least one mutant" $
    selectIfElseBoolNegOps (H.ast ifElseSrc) `shouldSatisfy` (not . null)
  it "negate-guards produces at least one mutant" $
    selectGuardedBoolNegOps (H.ast guardSrc) `shouldSatisfy` (not . null)
  it "remove-not produces at least one mutant" $
    selectRemoveNotOps (H.ast notSrc) `shouldSatisfy` (not . null)
  it "remove-negation produces at least one mutant" $
    selectRemoveNegationOps (H.ast negSrc) `shouldSatisfy` (not . null)
  it "case-alt-remove produces at least one mutant" $
    selectCaseAltRemoveOps (H.ast caseAltSrc) `shouldSatisfy` (not . null)
  it "case-default-remove produces at least one mutant" $
    selectCaseDefaultRemoveOps (H.ast caseDefaultSrc) `shouldSatisfy` (not . null)
  it "remove-stmt produces at least one mutant" $
    selectRemoveStmtOps (H.ast stmtSrc) `shouldSatisfy` (not . null)
  it "remove-let-binding produces at least one mutant" $
    selectRemoveLetBindingOps (H.ast letSrc) `shouldSatisfy` (not . null)
  it "remove-where-binding produces at least one mutant" $
    selectRemoveWhereBindingOps (H.ast whereSrc) `shouldSatisfy` (not . null)
  it "remove-self-assign produces at least one mutant" $
    selectRemoveSelfAssignOps (H.ast selfAssignSrc) `shouldSatisfy` (not . null)
  it "negate-literal produces at least one mutant" $
    selectNegateLiteralOps (H.ast negLitSrc) `shouldSatisfy` (not . null)
  it "string-literal produces at least one mutant" $
    selectStringLiteralOps (H.ast stringLitSrc) `shouldSatisfy` (not . null)
  it "bool-operand produces at least one mutant" $
    selectBoolOperandOps (H.ast boolOpSrc) `shouldSatisfy` (not . null)
  it "flip-maybe produces at least one mutant" $
    selectFlipMaybeOps (H.ast flipMaybeSrc) `shouldSatisfy` (not . null)
  it "flip-either produces at least one mutant" $
    selectFlipEitherOps (H.ast flipEitherSrc) `shouldSatisfy` (not . null)
  it "remove-forkIO produces at least one mutant" $
    selectRemoveForkIOOps (H.ast forkIOSrc) `shouldSatisfy` (not . null)
  it "bracket-degenerate produces at least one mutant" $
    selectBracketDegenerateOps (H.ast bracketSrc) `shouldSatisfy` (not . null)
  it "error-guard produces at least one mutant" $
    selectErrorGuardOps (H.ast errorGuardSrc) `shouldSatisfy` (not . null)
  it "replace-mutable-arg produces at least one mutant" $
    selectReplaceMutableArgOps (H.ast mutableArgSrc) `shouldSatisfy` (not . null)
  it "zero-return produces at least one mutant" $
    selectZeroReturnOps (H.ast zeroReturnSrc) `shouldSatisfy` (not . null)
