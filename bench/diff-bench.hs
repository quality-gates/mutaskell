module Main where

import Control.Exception (evaluate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Printf (printf)

import App.Output (unifiedDiff)

runDiff :: String -> String -> String
runDiff = unifiedDiff
{-# NOINLINE runDiff #-}

main :: IO ()
main = do
    [mode, sizeText] <- getArgs
    let size = read sizeText :: Int
        original = unlines [lineFor i | i <- [1 .. size]]
        mutated = unlines [mutatedLine mode size i | i <- [1 .. size]]
    _ <- evaluate (length original + length mutated)
    performGC
    stats0 <- getRTSStats
    alloc0 <- evaluate (allocated_bytes stats0)
    cpu0 <- getCPUTime
    wall0 <- getCurrentTime
    result <- evaluate (length (runDiff original mutated))
    wall1 <- getCurrentTime
    cpu1 <- getCPUTime
    performGC
    stats1 <- getRTSStats
    alloc1 <- evaluate (allocated_bytes stats1)
    live1 <- evaluate (max_live_bytes stats1)
    printf "%s n=%d output=%d cpu=%.6f wall=%.6f allocated_bytes=%s max_live_bytes=%s\n"
        mode size result
        (fromIntegral (cpu1 - cpu0) / 1e12 :: Double)
        (realToFrac (diffUTCTime wall1 wall0) :: Double)
        (show (alloc1 - alloc0))
        (show live1)

lineFor :: Int -> String
lineFor i = "line " ++ show i ++ " payload"

mutatedLine :: String -> Int -> Int -> String
mutatedLine "single" size i
    | i == size = "changed final line"
    | otherwise = lineFor i
mutatedLine "many" _ i
    | i `mod` 7 == 0 = "changed line " ++ show i
    | otherwise = lineFor i
mutatedLine mode _ _ = error ("mode must be single or many: " ++ mode)
