{-# LANGUAGE ScopedTypeVariables #-}

-- | Read the HPC Tix and Mix files.
module Test.Mutaskell.Tix where

import Control.Exception (catch, SomeException)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Ord (Down (..))
import System.IO.Unsafe (unsafePerformIO)
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
    atomicModifyIORef' tixReadCountRef (\n -> (n + 1, ()))
    tix <- readTix path
    case tix of
        Nothing -> return []
        Just (Tix tms) -> return tms

-- | Number of Tix-file reads performed by this process.  This is diagnostic
-- instrumentation used to verify that project runs reuse their parsed input.
tixReadCount :: IO Int
tixReadCount = readIORef tixReadCountRef

-- | Process-local counter backing 'tixReadCount'.
{-# NOINLINE tixReadCountRef #-}
tixReadCountRef :: IORef Int
tixReadCountRef = unsafePerformIO (newIORef 0)

-- | Parsed coverage modules indexed by every qualified-name suffix that can
-- match an unqualified source module name.
newtype TixIndex = TixIndex (Map.Map String [TixModule])

-- | Parse a tix file into a reusable project coverage index.
parseTixIndex :: String -> IO TixIndex
parseTixIndex path = buildTixIndex <$> parseTix path

-- | Build a reusable project coverage index from parsed tix modules.
buildTixIndex :: [TixModule] -> TixIndex
buildTixIndex tms = TixIndex $ Map.fromListWith (++)
    [ (key, [tm])
    | tm <- tms
    , key <- moduleNameKeys (tixModuleName tm)
    ]

-- | Include the full module name and each suffix after a package separator.
moduleNameKeys :: String -> [String]
moduleNameKeys name = name : case dropWhile (/= '/') name of
    []         -> []
    (_ : rest) -> if null rest then [] else moduleNameKeys rest

-- | Look up coverage for a module in an already parsed tix index.
getUnCoveredPatchesFromIndex :: TixIndex -> String -> IO (Either String (Maybe [Span]))
getUnCoveredPatchesFromIndex (TixIndex modules) name =
    case Map.findWithDefault [] name modules of
        [tm] -> do
            emix <- getMix tm
            case emix of
                Left err  -> return (Left err)
                Right mix ->
                    let (_, modSpan) = mixTix (tixModuleName tm) mix tm
                        uncovSpan    = filter (not . isCovered . snd) modSpan
                    in return $ Right $ Just $ removeRedundantSpans $ map fst uncovSpan
        _ -> return (Right Nothing)

-- | The line and column of one end of a coverage span.
type SpanPosition = (Int, Int)

-- | An ordered containment index for coverage spans.
newtype SpanIndex = SpanIndex (Map.Map SpanPosition SpanPosition)

-- | Build an ordered index whose prefix maxima answer span-containment queries.
indexSpans :: [Span] -> SpanIndex
indexSpans spans = SpanIndex (Map.fromAscList (prefixMaxima endpoints))
  where
    endpoints = Map.toAscList $ Map.fromListWith max
        [ (spanStartPosition sp, spanEndPosition sp) | sp <- spans ]

-- | Test whether a candidate span is contained in an indexed span.
spanIndexContains :: SpanIndex -> Span -> Bool
spanIndexContains (SpanIndex indexed) candidate =
    case Map.lookupLE (spanStartPosition candidate) indexed of
        Nothing          -> False
        Just (_, maxEnd) -> maxEnd >= spanEndPosition candidate

-- | Convert endpoint entries into prefix-maximum entries in one ordered pass.
prefixMaxima :: [(SpanPosition, SpanPosition)]
             -> [(SpanPosition, SpanPosition)]
prefixMaxima [] = []
prefixMaxima entries@((_, firstEnd) : _) =
    snd $ List.mapAccumL addMaximum firstEnd entries
  where
    addMaximum current (start, end) =
        let next = max current end
        in (next, (start, next))

-- | Read the corresponding Mix file to a TixModule.
-- Returns 'Left' with a user-readable message if the .mix file cannot be found.
getMix :: TixModule -> IO (Either String Mix)
getMix tm = do
    let name = tixModuleName tm
    -- Try reading with original name
    res <- tryReadMix [".hpc"] (Right tm)
    case res of
        Just m -> return (Right m)
        Nothing -> do
            -- Try stripping package prefix (everything before first slash)
            let strippedName = case break (== '/') name of
                    (_, "") -> name
                    (_, s) -> drop 1 s
            res2 <- tryReadMix [".hpc"] (Left strippedName)
            case res2 of
                Just m -> return (Right m)
                Nothing -> return $ Left $
                    "Coverage error: cannot find " ++ name
                    ++ " (or " ++ strippedName ++ ") in .hpc"
                    ++ " — is the test suite built with -fhpc?"

-- | Helper to try reading a mix file without crashing
tryReadMix :: [FilePath] -> Either String TixModule -> IO (Maybe Mix)
tryReadMix fp target = (Just <$> readMix fp target) `catch` (\(_ :: SomeException) -> return Nothing)

-- | return the tix and mix information, or a 'Left' error if any .mix file is missing.
getMixedTix :: String -> IO (Either String [(String, [(Span, TCovered)])])
getMixedTix file = do
    tms <- parseTix file
    eResults <- mapM getMix tms
    case sequence eResults of
        Left err -> return (Left err)
        Right mixs -> do
            let names = map tixModuleName tms
            return $ Right $ zipWith3 mixTix names mixs tms

{- | getUnCoveredPatches returns the largest parts of the named module that are
not covered.  Only the requested module's @.mix@ is read, so coverage data for a
multi-module @.tix@ (e.g. a cabal project whose test-suite modules' @.mix@ files
live in a different directory) still works — previously a single missing @.mix@
for /any/ module failed the whole lookup.  Returns 'Left' with a user-readable
error only if the requested module's own @.mix@ cannot be found.
-}
getUnCoveredPatches :: String -> String -> IO (Either String (Maybe [Span]))
getUnCoveredPatches file name =
    parseTixIndex file >>= (`getUnCoveredPatchesFromIndex` name)

-- | Does a tix module's name match the requested (unqualified) module name?
matchesName :: String -> TixModule -> Bool
matchesName name tm =
    let k = tixModuleName tm in name == k || (("/" ++ name) `List.isSuffixOf` k)

-- | Get the span and covering information of the given module
getNamedModule :: String -> [(String, [(Span, TCovered)])] -> [(Span, TCovered)]
getNamedModule mname val =
    case filter (\(k, _) -> mname == k || (("/" ++ mname) `List.isSuffixOf` k)) val of
        ((_, x) : _) -> x
        [] -> []

-- | Remove spans which are contained within others of same kind.
removeRedundantSpans :: [Span] -> [Span]
removeRedundantSpans spans =
    [ sp
    | (sp, index) <- zip spans [0 :: Int ..]
    , Set.member index survivors
    ]
  where
    ordered = List.sortOn orderKey (zip spans [0 :: Int ..])
    survivors = sweep ordered Nothing

    orderKey (sp, index) =
        (spanStartPosition sp, Down (spanEndPosition sp), index)

    sweep [] _ = Set.empty
    sweep entries@((firstSpan, _) : _) previousMaximum =
        let start = spanStartPosition firstSpan
            (sameStart, rest) = List.span
                ((== start) . spanStartPosition . fst) entries
            (groupMaximum, kept) =
                List.foldl' (mark previousMaximum) (Nothing, Set.empty) sameStart
            nextMaximum = maxMaybe previousMaximum groupMaximum
        in Set.union kept (sweep rest nextMaximum)

    mark previousMaximum (groupMaximum, kept) (sp, index) =
        let end = spanEndPosition sp
            redundant = maybe False (>= end) previousMaximum
                || maybe False (> end) groupMaximum
            nextGroupMaximum = Just $ maybe end (`max` end) groupMaximum
        in ( nextGroupMaximum
           , if redundant then kept else Set.insert index kept )

    maxMaybe Nothing y = y
    maxMaybe x Nothing = x
    maxMaybe (Just x) (Just y) = Just (max x y)

-- | Extract the 1-based line and column of a span's start.
spanStartPosition :: Span -> SpanPosition
spanStartPosition sp =
    let (line, column, _, _) = fromHpcPos sp
    in (line, column)

-- | Extract the 1-based line and column of a span's end.
spanEndPosition :: Span -> SpanPosition
spanEndPosition sp =
    let (_, _, line, column) = fromHpcPos sp
    in (line, column)
