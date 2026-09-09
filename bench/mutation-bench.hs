module Main where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Printf (printf)

import Test.Mutaskell.Config (Config (maxNumMutants), defaultConfig)
import Test.Mutaskell.MuOp (Module_)
import Test.Mutaskell.Mutation
import Test.Mutaskell.TestAdapter (Mutant (_mutant), toMutant)
import Test.Mutaskell.Utils.Common (apTh)
import Language.Haskell.GHC.ExactPrint (exactPrint)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fixture, sizeText] -> runFixture fixture (read sizeText)
        _ -> fail "usage: mutation-bench FIXTURE SIZE"

runFixture :: String -> Int -> IO ()
runFixture fixture size = case fixture of
    "phases" -> runPhases size
    _ -> runSelectorAndSampled fixture size

runSelectorAndSampled :: String -> Int -> IO ()
runSelectorAndSampled fixture size = do
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

{- | Report the generation pipeline's phases separately over a dense literal
module: selector work, operator sampling, AST application, mutator/span
deduplication, rendering and rendered-source deduplication.  The exact path
applies every candidate; the sampled path spends a cap of one operator, so the
two rows show where the candidate bound saves work.  Each phase forces its own
result, so a phase's time is its own work, not downstream forcing.
-}
runPhases :: Int -> IO ()
runPhases size = do
    parsed <- getASTFromStr (denseSource size)
    ast <- case parsed of
        Left err -> fail err
        Right value -> return value
    runPhasesMode size "exact" False defaultConfig ast
    runPhasesMode size "sampled" True (defaultConfig { maxNumMutants = 1 }) ast

runPhasesMode :: Int -> String -> Bool -> Config -> Module_ -> IO ()
runPhasesMode size mode sampled config ast = do
    (origSource, ops) <- phaseTime (mode ++ "/select") $ do
        let (source, ops) = prepareSelectorInputs config [] ast
            sizes = (length source, length ops)
        _ <- evaluate sizes
        return (source, ops)
    sampledOps <-
        if sampled
            then phaseTime (mode ++ "/opsample") (sampleOps config ops)
            else return ops
    applied <- phaseTime (mode ++ "/apply") $ do
        let applied = mutatesN sampledOps ast 1
        _ <- evaluate (sum (map (const (1 :: Int)) applied))
        return applied
    sites <- phaseTime (mode ++ "/sitededup") $ do
        let sites = nubOpSites applied
        _ <- evaluate (length sites)
        return sites
    rendered <- phaseTime (mode ++ "/render") $ do
        let rendered = map (toMutant . apTh exactPrint) sites
        count <- evaluate (sum (map (length . _mutant) rendered))
        return (rendered, count)
    timePhase "phases" size (mode ++ "/srcdedup") $ do
        let mutants = nubRendered (filter (\m -> _mutant m /= origSource) (fst rendered))
        forceMutants mutants

sourceFor :: String -> Int -> String
sourceFor fixture size = case fixture of
    "typed"  -> typedSource size
    "nested" -> nestedSource size
    _        -> error "fixture must be typed or nested"

-- | Time one generation phase.  The action forces and returns the value the
-- next phase consumes, so a phase's measured time is its own work.
phaseTime :: String -> IO a -> IO a
phaseTime phase action = do
    performGC
    start <- getCurrentTime
    result <- action
    finish <- getCurrentTime
    printf "phase=%s wall=%.6f\n" phase (realToFrac (diffUTCTime finish start) :: Double)
    return result

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
nestedIf 0 = "0"
nestedIf depth =
    "if x > " ++ show depth ++ " then " ++ nestedIf (depth - 1)
        ++ " else x"

denseSource :: Int -> String
denseSource n = unlines ("module Bench where" : ["f" ++ show i ++ " x = x + 1" | i <- [1 .. n]])
