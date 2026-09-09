module Main where

import Control.Exception (evaluate)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Printf (printf)

import Test.Mutaskell.Tix
    ( indexSpans
    , removeRedundantSpans
    , spanIndexContains
    , toSpan
    )

main :: IO ()
main = do
    [mode, size] <- getArgs
    let n = read size :: Int
        spans = [toSpan (i, 1, i, 2) | i <- [1 .. n]]
        action = case mode of
            "sweep" -> evaluate (length (removeRedundantSpans spans))
            "query" -> do
                let indexed = indexSpans spans
                evaluate (length (filter (spanIndexContains indexed) spans))
            _ -> error "mode must be sweep or query"
    performGC
    cpu0 <- getCPUTime
    result <- action
    cpu1 <- getCPUTime
    printf "%s n=%d result=%d cpu=%.6f\n" mode n result
        (fromIntegral (cpu1 - cpu0) / 1e12 :: Double)
