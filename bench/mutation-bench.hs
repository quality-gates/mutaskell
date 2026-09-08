module Main where

import Control.Exception (evaluate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Printf (printf)

import Test.Mutaskell.Config (Config (maxNumMutants), defaultConfig)
import Test.Mutaskell.Mutation
import Test.Mutaskell.TestAdapter (Mutant (_mutant))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fixture, sizeText] -> runFixture fixture (read sizeText)
        _ -> fail "usage: mutation-bench FIXTURE SIZE"

runFixture :: String -> Int -> IO ()
runFixture fixture size = do
    let source = sourceFor fixture size
    parsed <- getASTFromStr source
    ast <- case parsed of
        Left err -> fail err
        Right value -> return value
    let config = defaultConfig { maxNumMutants = 1 }
    timePhase fixture size "selector" $
        evaluate (length (applicableOps config ast))
    timePhase fixture size "sampled" $ do
        mutants <- genSampledMutants config ast
        forceMutants mutants

sourceFor :: String -> Int -> String
sourceFor fixture size = case fixture of
    "typed"  -> typedSource size
    "nested" -> nestedSource size
    _        -> error "fixture must be typed or nested"

timePhase :: String -> Int -> String -> IO Int -> IO ()
timePhase fixture size phase action = do
    performGC
    start <- getCurrentTime
    result <- action
    finish <- getCurrentTime
    printf "%s fixture=%s size=%d result=%d wall=%.6f\n"
        phase fixture size result
        (realToFrac (diffUTCTime finish start) :: Double)

forceMutants :: [Mutant] -> IO Int
forceMutants mutants = evaluate (sum (map (length . _mutant) mutants))

typedSource :: Int -> String
typedSource n = unlines $ "module Bench where" : concat
    [ [ "f" ++ show i ++ " :: Int -> Int"
      , "f" ++ show i ++ " x = x + " ++ show i
      ]
    | i <- [1 .. n]
    ]

nestedSource :: Int -> String
nestedSource n = unlines $
    [ "module Bench where"
    , "f x = case x of"
    ] ++ [ "  " ++ show i ++ " -> " ++ nestedIf n | i <- [1 .. n] ]

nestedIf :: Int -> String
nestedIf depth = go depth
  where
    go 0 = "0"
    go level =
        "if x > " ++ show level ++ " then " ++ go (level - 1)
            ++ " else x"
