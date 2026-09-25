-- | Pure coverage index and interval containment algorithms.
module Test.Mutaskell.CoverageIndex
    ( Span
    , toSpan
    , spanStartLine
    , insideSpan
    , SpanPosition
    , spanStartPosition
    , spanEndPosition
    , SpanIndex (..)
    , indexSpans
    , spanIndexContains
    , removeRedundantSpans
    , ModuleCoverage (..)
    , CoverageIndex (..)
    , moduleNameKeys
    , emptyCoverageIndex
    , fromModuleResults
    , fromModuleSpans
    , fromSpans
    , lookupModuleSpans
    , uncoveredSpans
    , isCovered
    ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Trace.Hpc.Util (HpcPos, fromHpcPos, insideHpcPos, toHpcPos)

-- | Source code span coordinate, matching 'HpcPos'.
type Span = HpcPos

-- | Convert coordinates to a span.
toSpan :: (Int, Int, Int, Int) -> Span
toSpan = toHpcPos

-- | Start line of a span.
spanStartLine :: Span -> Int
spanStartLine sp = let (l, _, _, _) = fromHpcPos sp in l

-- | Check whether the first span is inside the second span.
insideSpan :: Span -> Span -> Bool
insideSpan = insideHpcPos

-- | Line and column of a span boundary.
type SpanPosition = (Int, Int)

-- | Start position of a span.
spanStartPosition :: Span -> SpanPosition
spanStartPosition sp =
    let (line, column, _, _) = fromHpcPos sp
    in (line, column)

-- | End position of a span.
spanEndPosition :: Span -> SpanPosition
spanEndPosition sp =
    let (_, _, line, column) = fromHpcPos sp
    in (line, column)

-- | Ordered containment index for coverage spans.
newtype SpanIndex = SpanIndex (Map.Map SpanPosition SpanPosition)
    deriving (Eq, Show)

-- | Build an ordered index whose prefix maxima answer span containment queries.
indexSpans :: [Span] -> SpanIndex
indexSpans spans = SpanIndex (Map.fromAscList (prefixMaxima endpoints))
  where
    endpoints = Map.toAscList $ Map.fromListWith max
        [ (spanStartPosition sp, spanEndPosition sp) | sp <- spans ]

-- | Check whether a candidate span is inside an indexed span.
spanIndexContains :: SpanIndex -> Span -> Bool
spanIndexContains (SpanIndex indexed) candidate =
    case Map.lookupLE (spanStartPosition candidate) indexed of
        Nothing          -> False
        Just (_, maxEnd) -> maxEnd >= spanEndPosition candidate

-- | Convert endpoint entries to prefix maximum entries in one ordered pass.
prefixMaxima :: [(SpanPosition, SpanPosition)]
             -> [(SpanPosition, SpanPosition)]
prefixMaxima [] = []
prefixMaxima entries@((_, firstEnd) : _) =
    snd $ List.mapAccumL addMaximum firstEnd entries
  where
    addMaximum current (start, end) =
        let next = max current end
        in (next, (start, next))

-- | Remove spans that are contained inside other spans in the list.
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

-- | Coverage data for a single module.
data ModuleCoverage
    = ModuleCoverage
        { mcSpans :: ![Span]
        , mcIndex :: !SpanIndex
        }
    | ModuleError !String
    deriving (Eq, Show)

-- | Pure index of module coverage spans.
newtype CoverageIndex = CoverageIndex
    { ciModules :: Map.Map String [ModuleCoverage]
    } deriving (Eq, Show)

-- | Extract qualified and unqualified suffix keys for a module name.
moduleNameKeys :: String -> [String]
moduleNameKeys ""   = [""]
moduleNameKeys name = name : case dropWhile (/= '/') name of
    []         -> []
    (_ : rest) -> if null rest then [] else moduleNameKeys rest

-- | Empty coverage index where all spans are considered covered.
emptyCoverageIndex :: CoverageIndex
emptyCoverageIndex = CoverageIndex Map.empty

-- | Construct a 'CoverageIndex' from module results.
fromModuleResults :: [(String, Either String [Span])] -> CoverageIndex
fromModuleResults results = CoverageIndex $ Map.fromListWith (++)
    [ (key, [mc])
    | (name, res) <- results
    , let mc = case res of
            Left err    -> ModuleError err
            Right spans ->
                let clean = removeRedundantSpans spans
                in ModuleCoverage clean (indexSpans clean)
    , key <- moduleNameKeys name
    ]

-- | Construct a 'CoverageIndex' from module names and uncovered spans.
fromModuleSpans :: [(String, [Span])] -> CoverageIndex
fromModuleSpans ms = fromModuleResults [ (name, Right spans) | (name, spans) <- ms ]

-- | Construct a single-module or anonymous 'CoverageIndex' from uncovered spans.
fromSpans :: [Span] -> CoverageIndex
fromSpans spans = fromModuleSpans [("", spans)]

-- | Resolve candidate module coverages for a module name.
resolveCandidates :: CoverageIndex -> String -> [ModuleCoverage]
resolveCandidates (CoverageIndex mods) name =
    case Map.lookup name mods of
        Just cs@(_:_) -> cs
        _             -> Map.findWithDefault [] "" mods

-- | Look up uncovered spans for a module name:
-- 'Left' on error,
-- 'Right (Just spans)' when matched,
-- 'Right Nothing' when unmatched or ambiguous.
lookupModuleSpans :: CoverageIndex -> String -> Either String (Maybe [Span])
lookupModuleSpans ci name =
    case resolveCandidates ci name of
        [ModuleCoverage spans _] -> Right (Just spans)
        [ModuleError err]        -> Left err
        _                        -> Right Nothing

-- | Return uncovered spans for a module, or empty list when unmatched.
uncoveredSpans :: CoverageIndex -> String -> [Span]
uncoveredSpans ci name =
    case resolveCandidates ci name of
        [ModuleCoverage spans _] -> spans
        _                        -> []

-- | Check whether a candidate span in a module is covered.
isCovered :: CoverageIndex -> String -> Span -> Bool
isCovered ci name candidate =
    case resolveCandidates ci name of
        [ModuleCoverage _ sidx] -> not (spanIndexContains sidx candidate)
        _                       -> True
