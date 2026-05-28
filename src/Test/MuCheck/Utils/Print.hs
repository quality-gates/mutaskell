{-# LANGUAGE TupleSections #-}

-- | Common print utilities
module Test.MuCheck.Utils.Print where

import Data.List (intercalate)
import Debug.Trace

import GHC.IO.Handle
import System.Directory
import System.Environment
import System.IO
import System.IO.Temp (withSystemTempFile)

-- | simple wrapper for adding a % at the end.
(./.) :: (Show a, Integral a) => a -> a -> String
n ./. t | t > 0 = "(" ++ show (n * 100 `div` t) ++ "%)"
n ./. t = "(" ++ show n ++ "/" ++ show t ++ ")"

-- | join lines together
showAS :: [String] -> String
showAS = intercalate "\n"

-- | make lists into lines in text.
showA :: (Show a) => [a] -> String
showA = showAS . map show

-- | convenience function for debug
tt :: (Show a) => a -> a
tt v = trace (">" ++ show v) v

-- | Capture output and err of an IO action
catchOutputStr :: IO a -> IO (a, String)
catchOutputStr f = do
    isdebug <- lookupEnv "MuDEBUG"
    case isdebug of
        Just _ -> fmap (,"") f
        Nothing -> withSystemTempFile "_mucheck" $ \tmpf tmph -> do
            res <- redirectToHandle f tmph
            str <- readFile tmpf
            removeFile tmpf
            return (res, str)

-- | Capture output and err of an IO action to a file
catchOutput :: String -> IO a -> IO a
catchOutput fn f = do
    isdebug <- lookupEnv "MuDEBUG"
    case isdebug of
        Just _ -> f
        Nothing -> withFile fn WriteMode (redirectToHandle f)

-- | Redirect out and err to handle
redirectToHandle :: IO b -> Handle -> IO b
redirectToHandle f tmph = do
    stdout_dup <- hDuplicate stdout
    stderr_dup <- hDuplicate stderr
    hDuplicateTo tmph stdout
    hDuplicateTo tmph stderr
    hClose tmph
    res <- f
    hDuplicateTo stdout_dup stdout
    hDuplicateTo stderr_dup stderr
    return res

-- | Conditionally print a message if MuDEBUG is set
say :: String -> IO ()
say str = do
    isdebug <- lookupEnv "MuDEBUG"
    case isdebug of
        Just _ -> putStrLn str
        _ -> return ()
