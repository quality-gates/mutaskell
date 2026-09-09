module Main where

import Control.Exception (evaluate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Printf (printf)

import Test.Mutaskell.Config (Config (maxNumMutants), defaultConfig)
import Test.Mutaskell.Mutation (genSampledMutants, getASTFromStr, removeOneElem)
import Test.Mutaskell.TestAdapter (Mutant (..))

-- Do not let the consumer fuse with removeOneElem. Work must stay visible.
runDirect :: [Int] -> [[Int]]
runDirect = removeOneElem
{-# NOINLINE runDirect #-}

-- The pre-fix enumeration, kept for comparison: request every
-- size-(c-1) combination of the input instead of the single deletions.
legacyRemoveOneElem :: [Int] -> [[Int]]
legacyRemoveOneElem l = choose l (length l - 1)
  where
    choose _ 0 = [[]]
    choose [] _ = []
    choose (x : xs) n = map (x :) (choose xs (n - 1)) ++ choose xs n
{-# NOINLINE legacyRemoveOneElem #-}

-- A literal list literal of n elements, the workload shape that drives
-- explicit-list removal.
listSource :: Int -> String
listSource n = "module Dense where\nxs = [" ++ body ++ "]\n"
  where
    body = unwords (replicate (n - 1) "1," ++ ["1"])

main :: IO ()
main = do
    [mode, size] <- getArgs
    let n = read size :: Int
    action <- case mode of
        "direct" -> pure $ do
            got <- evaluate (runDirect [1 .. n])
            evaluate (sum (map length got))
        "legacy" -> pure $ do
            got <- evaluate (legacyRemoveOneElem [1 .. n])
            evaluate (sum (map length got))
        "sampled" -> do
            ast <- either fail pure =<< getASTFromStr (listSource n)
            pure $ do
                ms <- genSampledMutants
                    (defaultConfig { maxNumMutants = 1 }) ast
                evaluate (sum (map (length . _mutant) ms))
        _ -> fail "mode must be direct, legacy or sampled"
    performGC
    stats0 <- getRTSStats
    alloc0 <- evaluate (allocated_bytes stats0)
    cpu0 <- getCPUTime
    wall0 <- getCurrentTime
    result <- action
    wall1 <- getCurrentTime
    cpu1 <- getCPUTime
    performGC
    stats1 <- getRTSStats
    alloc1 <- evaluate (allocated_bytes stats1)
    printf
        "%s n=%d result=%d cpu=%.6f wall=%.6f allocated_bytes=%s\n"
        mode
        n
        (result :: Int)
        (fromIntegral (cpu1 - cpu0) / 1e12 :: Double)
        (realToFrac (diffUTCTime wall1 wall0) :: Double)
        (show (alloc1 - alloc0))