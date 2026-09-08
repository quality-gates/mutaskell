module Main where

import Control.Exception (evaluate)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)
import Text.Printf (printf)

import App.Filter
    ( applyAnnotations
    , applyBaseline
    , applyIgnoreLines
    , applyRunMutantId
    , cacheMutantIds
    , indexAnnotations
    , indexChangedLines
    , indexIds
    , indexSourceLines
    )
import Test.Mutaskell.Config (MuVar (..))
import Test.Mutaskell.TestAdapter (Mutant (..))
import Test.Mutaskell.Tix (toSpan)
import Test.Mutaskell.Utils.Common (hash)

mkMutants :: Int -> Int -> [Mutant]
mkMutants k lineCount =
    [ Mutant
        { _mutant = show i ++ replicate 128 'x'
        , _mtype  = if even i then MutateValues else MutateFunctions
        , _mspan  = toSpan (line, 1, line, 8)
        }
    | i <- [1 .. k]
    , let line = (i `mod` max 1 lineCount) + 1
    ]

mkSrc :: Int -> String
mkSrc n = unlines ["line " ++ show i ++ " payload" | i <- [1 .. n]]

mkAnns :: Int -> [(Int, [String])]
mkAnns n = [(i, if even i then ["literal-values"] else []) | i <- [1 .. n]]

mkIdList :: Int -> [String]
mkIdList n = [hash (show i ++ replicate 128 'x') | i <- [1 .. n]]

timeIt :: String -> Int -> Int -> IO Int -> IO ()
timeIt mode k n action = do
    performGC
    wall0 <- getCurrentTime
    result <- action
    wall1 <- getCurrentTime
    printf "%s k=%d n=%d result=%d wall=%.6f\n" mode k n result
        (realToFrac (diffUTCTime wall1 wall0) :: Double)

main :: IO ()
main = do
    args <- getArgs
    (mode, k, n) <- case args of
        [m, ks, ns] -> return (m, read ks, read ns)
        _           -> fail "usage: filter-bench MODE K N"
    let ms  = mkMutants k n
        src = mkSrc n
        anns = mkAnns n
        ids = mkIdList n
    case mode of
        "index-ids" -> do
            _ <- evaluate (length ids)
            timeIt mode k n $ evaluate (Set.size (indexIds ids))
        "index-anns" -> do
            _ <- evaluate (length anns)
            timeIt mode k n $ evaluate (IntMap.size (indexAnnotations anns))
        "index-lines" -> do
            let ls = [1 .. n]
            _ <- evaluate (length ls)
            timeIt mode k n $ evaluate (IntSet.size (indexChangedLines ls))
        "index-src" -> do
            _ <- evaluate (length src)
            timeIt mode k n $ evaluate (IntMap.size (indexSourceLines src))
        "cache-hash" -> do
            _ <- evaluate (length ms)
            timeIt mode k n $ evaluate (length (cacheMutantIds ms))
        "empty-ann" -> do
            _ <- evaluate (length ms)
            timeIt mode k n $ evaluate (length (applyAnnotations [] ms))
        "empty-ignore" -> do
            _ <- evaluate (length ms)
            timeIt mode k n $ evaluate (length (applyIgnoreLines src [] ms))
        "empty-id" -> do
            _ <- evaluate (length ms)
            timeIt mode k n $ evaluate (length (applyRunMutantId Nothing ms))
        "empty-base" -> do
            _ <- evaluate (length ms)
            timeIt mode k n $ fmap length (applyBaseline Nothing ms)
        "filter-ann" -> do
            _ <- evaluate (length ms + length anns)
            timeIt mode k n $ evaluate (length (applyAnnotations anns ms))
        "filter-ignore" -> do
            _ <- evaluate (length ms + length src)
            timeIt mode k n $ evaluate (length (applyIgnoreLines src ["payload"] ms))
        "filter-id" -> do
            _ <- evaluate (length ms)
            let mid = case ms of
                    (m:_) -> hash (_mutant m)
                    []    -> ""
            timeIt mode k n $ evaluate (length (applyRunMutantId (Just mid) ms))
        "filter-ids" -> do
            _ <- evaluate (length ms + length ids)
            withSystemTempDirectory "mutaskell-filter-bench" $ \dir -> do
                let path = dir ++ "/ids"
                writeFile path (unlines ids)
                timeIt mode k n $ fmap length (applyBaseline (Just path) ms)
        _ -> fail ("unknown mode: " ++ mode)
