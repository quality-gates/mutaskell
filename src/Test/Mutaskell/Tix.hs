{-# LANGUAGE ScopedTypeVariables #-}

-- | Read HPC Tix and Mix files to build coverage data.
module Test.Mutaskell.Tix
    ( -- * Re-exported pure coverage index interface
      Span
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

      -- * HPC file parsing and adapter
    , TCovered (..)
    , isTCovered
    , mixTix
    , parseTix
    , defaultMixPaths
    , loadCoverageIndex
    , getMix
    , tryReadMix
    , getMixedTix
    , getMixedTixWith
    , getUnCoveredPatches
    , getUnCoveredPatchesWith
    , getUnCoveredPatchesFromIndex
    , matchesName
    , getNamedModule

      -- * Backwards compatibility aliases
    , TixIndex
    , parseTixIndex
    , buildTixIndex
    ) where

import Control.Exception (SomeException, catch, evaluate, try)
import Control.Monad (forM)
import qualified Data.List as List
import System.Directory (doesFileExist)
import Trace.Hpc.Mix (Mix (..), readMix)
import Trace.Hpc.Tix
import Trace.Hpc.Util (fromHpcPos)

import Test.Mutaskell.CoverageIndex
    ( CoverageIndex (..)
    , ModuleCoverage (..)
    , Span
    , SpanIndex (..)
    , SpanPosition
    , emptyCoverageIndex
    , fromModuleResults
    , fromModuleSpans
    , fromSpans
    , indexSpans
    , insideSpan
    , isCovered
    , lookupModuleSpans
    , moduleNameKeys
    , removeRedundantSpans
    , spanEndPosition
    , spanIndexContains
    , spanStartLine
    , spanStartPosition
    , toSpan
    , uncoveredSpans
    )

-- | Whether a line is covered or not in HPC.
data TCovered
    = TCovered
    | TNotCovered
    deriving (Eq, Show)

-- | Check whether an HPC coverage tag represents covered code.
isTCovered :: TCovered -> Bool
isTCovered TCovered = True
isTCovered _        = False

-- | Default search directories for HPC @.mix@ files.
defaultMixPaths :: [FilePath]
defaultMixPaths = [".hpc"]

-- | Combine module location entries and execution tick counts.
mixTix :: String -> Mix -> TixModule -> (String, [(Span, TCovered)])
mixTix s (Mix _fp _int _h _i mixEntry) tix = (s, zipWith toLocC mymixes mytixes)
  where
    mytixes = tixModuleTixs tix
    mymixes = mixEntry
    toLocC (hpos, _) covT = (toSpan (fromHpcPos hpos), isCov covT)
    isCov 0 = TNotCovered
    isCov _ = TCovered

-- | Read a @.tix@ file. Returns 'Left' naming the path if the file does not
-- exist or cannot be parsed.
parseTix :: String -> IO (Either String [TixModule])
parseTix path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left $ "Coverage error: tix file not found: " ++ path
        else do
            tix <- try (readTix path >>= traverse evaluate)
            return $ case tix of
                Right (Just (Tix tms))    -> Right tms
                Right Nothing             -> Left $ unparseable "file could not be read"
                Left (e :: SomeException) -> Left $ unparseable (takeWhile (/= '\n') (show e))
  where
    unparseable why =
        "Coverage error: cannot parse tix file " ++ path ++ " (" ++ why ++ ")"

-- | Read the corresponding Mix file for a 'TixModule' using the provided search directories.
-- Returns 'Left' with a user-readable message if the @.mix@ file cannot be found.
getMix :: [FilePath] -> TixModule -> IO (Either String Mix)
getMix mixPaths tm = do
    let name = tixModuleName tm
    -- Try reading with original name
    res <- tryReadMix mixPaths (Right tm)
    case res of
        Just m -> return (Right m)
        Nothing -> do
            -- Try stripping package prefix (everything before first slash)
            let strippedName = case break (== '/') name of
                    (_, "") -> name
                    (_, s)  -> drop 1 s
            res2 <- tryReadMix mixPaths (Left strippedName)
            case res2 of
                Just m  -> return (Right m)
                Nothing -> return $ Left $
                    "Coverage error: cannot find " ++ name
                    ++ " (or " ++ strippedName ++ ") in " ++ dirDesc
                    ++ " — is the test suite built with -fhpc?"
  where
    dirDesc = case mixPaths of
        [d] -> d
        ds  -> List.intercalate ", " ds

-- | Try reading a mix file without crashing on exceptions.
tryReadMix :: [FilePath] -> Either String TixModule -> IO (Maybe Mix)
tryReadMix fp target = (Just <$> readMix fp target) `catch` (\(_ :: SomeException) -> return Nothing)

-- | Build a pure 'CoverageIndex' from an HPC @.tix@ file and search directories for @.mix@ files.
loadCoverageIndex :: [FilePath] -> FilePath -> IO (Either String CoverageIndex)
loadCoverageIndex mixPaths path = do
    etms <- parseTix path
    case etms of
        Left err  -> return (Left err)
        Right tms -> do
            results <- forM tms $ \tm -> do
                let name = tixModuleName tm
                emix <- getMix mixPaths tm
                case emix of
                    Left err -> return (name, Left err)
                    Right mix -> do
                        let (_, modSpan) = mixTix name mix tm
                            uncovSpan    = filter (not . isTCovered . snd) modSpan
                        return (name, Right (map fst uncovSpan))
            return (Right (fromModuleResults results))

-- | Return the tix and mix information using default mix paths, or 'Left' if any @.mix@ file is missing.
getMixedTix :: String -> IO (Either String [(String, [(Span, TCovered)])])
getMixedTix = getMixedTixWith defaultMixPaths

-- | Return the tix and mix information using the specified mix search paths.
getMixedTixWith :: [FilePath] -> String -> IO (Either String [(String, [(Span, TCovered)])])
getMixedTixWith mixPaths file = do
    etms <- parseTix file
    case etms of
        Left err  -> return (Left err)
        Right tms -> do
            eResults <- mapM (getMix mixPaths) tms
            case sequence eResults of
                Left err   -> return (Left err)
                Right mixs -> do
                    let names = map tixModuleName tms
                    return $ Right $ zipWith3 mixTix names mixs tms

-- | Get uncovered spans for the named module using default mix search paths.
-- An empty path means no coverage was requested.
getUnCoveredPatches :: String -> String -> IO (Either String (Maybe [Span]))
getUnCoveredPatches = getUnCoveredPatchesWith defaultMixPaths

-- | Get uncovered spans for the named module using specified mix search paths.
getUnCoveredPatchesWith :: [FilePath] -> String -> String -> IO (Either String (Maybe [Span]))
getUnCoveredPatchesWith _ "" _ = return (Right Nothing)
getUnCoveredPatchesWith mixPaths file name = do
    eindex <- loadCoverageIndex mixPaths file
    case eindex of
        Left err    -> return (Left err)
        Right index -> return (lookupModuleSpans index name)

-- | Look up coverage for a module in an already parsed coverage index.
getUnCoveredPatchesFromIndex :: CoverageIndex -> String -> IO (Either String (Maybe [Span]))
getUnCoveredPatchesFromIndex index name = return (lookupModuleSpans index name)

-- | Check whether a tix module name matches the requested module name.
matchesName :: String -> TixModule -> Bool
matchesName name tm =
    let k = tixModuleName tm in name == k || (("/" ++ name) `List.isSuffixOf` k)

-- | Get the span and covering information of the given module.
getNamedModule :: String -> [(String, [(Span, TCovered)])] -> [(Span, TCovered)]
getNamedModule mname val =
    case filter (\(k, _) -> mname == k || (("/" ++ mname) `List.isSuffixOf` k)) val of
        ((_, x) : _) -> x
        []           -> []

-- ---------------------------------------------------------------------------
-- Backwards compatibility aliases
-- ---------------------------------------------------------------------------

-- | Reusable coverage index.
type TixIndex = CoverageIndex

-- | Parse a tix file into a reusable coverage index using default mix search paths.
parseTixIndex :: String -> IO (Either String CoverageIndex)
parseTixIndex = loadCoverageIndex defaultMixPaths

-- | Build a reusable coverage index from parsed tix modules without mix files.
buildTixIndex :: [TixModule] -> CoverageIndex
buildTixIndex tms = fromModuleResults
    [ (tixModuleName tm, Left ("Coverage error: cannot resolve mix for " ++ tixModuleName tm ++ " without loadCoverageIndex"))
    | tm <- tms
    ]
