{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Read the HPC Tix and Mix files.
module Test.MuCheck.Tix where

import Control.Exception (catch, SomeException)
import Data.List (isSuffixOf)
import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Util

-- | Span info - same as HpcPos
type Span = HpcPos

-- | Convert a 4-tuple to a span
toSpan :: (Int, Int, Int, Int) -> Span
toSpan = toHpcPos

-- | Extract the 1-based start line from a span.
spanStartLine :: Span -> Int
spanStartLine sp = let (l, _, _, _) = fromHpcPos sp in l

-- | Whether a line is covered or not
data TCovered
    = TCovered
    | TNotCovered
    deriving (Eq, Show)

-- | Whether a line is covered or not
isCovered :: TCovered -> Bool
isCovered TCovered = True
isCovered _ = False

-- | insideSpan small big
insideSpan :: Span -> Span -> Bool
insideSpan = insideHpcPos

-- | `mixTix` joins together the location and coverage data.
mixTix :: String -> Mix -> TixModule -> (String, [(Span, TCovered)])
mixTix s (Mix _fp _int _h _i mixEntry) tix = (s, zipWith toLocC mymixes mytixes)
  where
    mytixes = tixModuleTixs tix
    mymixes = mixEntry
    toLocC (hpos, _) covT = (toSpan (fromHpcPos hpos), isCov covT)
    isCov 0 = TNotCovered
    isCov _ = TCovered

{- | reads a tix file. The tix is named for the binary run, and contains a list
of modules involved.
-}
parseTix :: String -> IO [TixModule]
parseTix path = do
    tix <- readTix path
    case tix of
        Nothing -> return []
        Just (Tix tms) -> return tms

-- | Read the corresponding Mix file to a TixModule
getMix :: TixModule -> IO Mix
getMix tm = do
    let name = tixModuleName tm
    -- Try reading with original name
    res <- tryReadMix [".hpc"] (Right tm)
    case res of
        Just m -> return m
        Nothing -> do
            -- Try stripping package prefix (everything before first slash)
            let strippedName = case break (== '/') name of
                    (_, "") -> name
                    (_, s) -> drop 1 s
            res2 <- tryReadMix [".hpc"] (Left strippedName)
            case res2 of
                Just m -> return m
                Nothing -> error $ "mucheck: can not find " ++ name ++ " (or " ++ strippedName ++ ") in .hpc"

-- | Helper to try reading a mix file without crashing
tryReadMix :: [FilePath] -> Either String TixModule -> IO (Maybe Mix)
tryReadMix fp target = (Just <$> readMix fp target) `catch` (\(_ :: SomeException) -> return Nothing)

-- | return the tix and mix information
getMixedTix :: String -> IO [(String, [(Span, TCovered)])]
getMixedTix file = do
    tms <- parseTix file
    mixs <- mapM getMix tms
    let names = map tixModuleName tms
    return $ zipWith3 mixTix names mixs tms

{- | getUnCoveredPatches returns the largest parts of the program that are not
covered.
-}
getUnCoveredPatches :: String -> String -> IO (Maybe [Span])
getUnCoveredPatches file name = do
    val <- getMixedTix file
    let modSpan = getNamedModule name val
        uncovSpan = filter (not . isCovered . snd) modSpan
    return $ case val of
        [] -> Nothing
        _ -> Just $ removeRedundantSpans $ map fst uncovSpan

-- | Get the span and covering information of the given module
getNamedModule :: String -> [(String, [(Span, TCovered)])] -> [(Span, TCovered)]
getNamedModule mname val =
    case filter (\(k, _) -> mname == k || (("/" ++ mname) `isSuffixOf` k)) val of
        ((_, x) : _) -> x
        [] -> []

-- | Remove spans which are contained within others of same kind.
removeRedundantSpans :: [Span] -> [Span]
removeRedundantSpans spans = filter (\s -> not $ any (\s' -> s /= s' && insideSpan s s') spans) spans
