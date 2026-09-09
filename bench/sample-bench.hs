module Main where

import Control.Exception (evaluate)
import Data.List (foldl')
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Mem (performGC)
import System.Random (mkStdGen)
import Text.Printf (printf)

import Test.Mutaskell.Utils.Common (sample)

-- Do not let the consumer fuse with sample. Allocation must stay visible.
runSample :: Int -> [Int] -> [Int]
runSample q xs = sample (mkStdGen 42) q xs
{-# NOINLINE runSample #-}

main :: IO ()
main = do
    [mode, size] <- getArgs
    let n = read size :: Int
        q = case mode of
            "half" -> n `div` 2
            "cap" -> 300
            _ -> error "mode must be half or cap"
        xs = [1 .. n] :: [Int]
    _ <- evaluate (foldl' (+) 0 xs)
    performGC
    stats0 <- getRTSStats
    alloc0 <- evaluate (allocated_bytes stats0)
    cpu0 <- getCPUTime
    wall0 <- getCurrentTime
    let got = runSample q xs
    _ <- evaluate (length got)
    result <- evaluate (foldl' (+) 0 got)
    wall1 <- getCurrentTime
    cpu1 <- getCPUTime
    performGC
    stats1 <- getRTSStats
    alloc1 <- evaluate (allocated_bytes stats1)
    live1 <- evaluate (max_live_bytes stats1)
    printf
        "%s n=%d q=%d result=%d cpu=%.6f wall=%.6f allocated_bytes=%s max_live_bytes=%s\n"
        mode
        n
        q
        result
        (fromIntegral (cpu1 - cpu0) / 1e12 :: Double)
        (realToFrac (diffUTCTime wall1 wall0) :: Double)
        (show (alloc1 - alloc0))
        (show live1)
