{-# LANGUAGE QuasiQuotes #-}

module Test.Muskell.MutationSpec.Helpers where

import Here
import Test.Muskell.Mutation
import Test.Muskell.MuOp (Module_, Decl_)

_myprop :: String
_myprop =
    [e|
module Prop where
import Test.QuickCheck

myFn [] = 0
myFn (x:xs) = 1 + myFn xs

{-# ANN myProp1 "Test" #-}
myProp1 xs = myFn [] == 0

{-# ANN myProp2 "Test" #-}
myProp2 xs = myFn [1,2,3] == 3
|]

_myprop_noann :: String
_myprop_noann =
    [e|
module Prop where
import Test.QuickCheck

myFn [] = 0
myFn (x:xs) = 1 + myFn xs
|]

_qc :: String
_qc =
    [e|
module Examples.QuickCheckTest where
import Test.QuickCheck
import Data.List

qsort :: [Int] -> [Int]
qsort [] = [1]
qsort (x:xs) = [2]

{-# ANN idEmpProp "Test" #-}
idEmpProp xs = qsort xs == qsort (qsort xs)

{-# ANN revProp "Test" #-}
revProp xs = qsort xs == qsort (reverse xs)

{-# ANN modelProp "Test" #-}
modelProp xs = qsort xs == sort xs
|]

_fullqc :: String
_fullqc =
    [e|
module Examples.QuickCheckTest where
import Test.QuickCheck
import Data.List

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort l ++ [x] ++ qsort r
    where l = filter (< x) xs
          r = filter (>= x) xs

{-# ANN idEmpProp "Test" #-}
idEmpProp xs = qsort xs == qsort (qsort xs)

{-# ANN revProp "Test" #-}
revProp xs = qsort xs == qsort (reverse xs)

{-# ANN modelProp "Test" #-}
modelProp xs = qsort xs == sort xs
|]

-- | Parse a Haskell source string into an AST for test use.
-- Calls 'error' on parse failure so test failures are visible.
ast :: String -> IO Module_
ast s = do
    result <- getASTFromStr s
    case result of
        Right a  -> return a
        Left err -> error $ "Test AST parse failure: " ++ err

decl :: Module_ -> [Decl_]
decl = getDecl
