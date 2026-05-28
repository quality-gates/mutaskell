-- | Example module for MuCheck mutation testing.
--
-- MuCheck discovers test functions in two ways:
--
-- 1. By naming convention: functions whose names start with @prop_@, @test_@,
--    or @spec_@ are picked up automatically — no annotation required.
--
-- 2. By annotation: any function annotated with @{-# ANN funcName "Test" #-}@
--    is treated as a test regardless of its name.  Use this as an opt-in
--    override for names that do not follow a convention.
--
-- This file demonstrates both paths.
module Examples.AssertCheckTest where

import Test.MuCheck.TestAdapter.AssertCheck

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x : xs) = qsort l ++ [x] ++ qsort r
  where
    l = filter (< x) xs
    r = filter (>= x) xs

uncoveredDummy :: Int -> Int
uncoveredDummy a = 0 + a

-- Discovered by naming convention (test_ prefix).
test_sortEmpty = assertCheck $ null (qsort [])

-- Discovered by naming convention (test_ prefix).
test_sortSorted = assertCheck $ qsort [1, 2, 3, 4] == [1, 2, 3, 4]

-- Discovered by naming convention (prop_ prefix — acts as a property test).
prop_sortIsIdempotent = assertCheck $ qsort (qsort [4, 3, 2, 1]) == qsort [4, 3, 2, 1]

-- Discovered by explicit annotation (legacy path; still fully supported).
{-# ANN sortSame "Test" #-}
sortSame = assertCheck $ qsort [1, 1, 1, 1] == [1, 1, 1, 1]

-- Discovered by naming convention (test_ prefix).
test_sortNeg = assertCheck $ qsort [-1, -2, 3] == [-2, -1, 3]
