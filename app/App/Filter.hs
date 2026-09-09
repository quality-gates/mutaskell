-- | Filter stages applied to the mutant list before evaluation.
-- Each stage receives the full list and returns a (possibly smaller) subset.
module App.Filter
  ( matchesPat
  , applyDisableEnable
  , applyAnnotations
  , applyBaseline
  , applyBlacklist
  , applyDiffLines
  , applyIgnoreLines
  , applyRunMutantId
  , parseAnnotations
  , checkGitDiff
  , parseDiffChangedLines
  , indexIds
  , indexAnnotations
  , indexChangedLines
  , indexSourceLines
  , cacheMutantIds
  , operatorSamplingEligible
  , applyBaselineCached
  , applyBlacklistCached
  , applyDiffLinesCached
  , applyIgnoreLinesCached
  , applyRunMutantIdCached
  ) where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, stripPrefix)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)

import App.Opts (Opts (..), splitOn)
import Test.Mutaskell.Config (showMuVar)
import Test.Mutaskell.TestAdapter (Mutant(..))
import Test.Mutaskell.Tix (spanStartLine)
import Test.Mutaskell.Utils.Common (hash)

-- | Is this file run eligible for operator-first mutant sampling?
--
-- Eligible runs sample mutation operators before rendering, so only the
-- sampled operators are applied, rendered and deduplicated.  A run is not
-- eligible when it needs the full candidate list: coverage reporting,
-- baseline, blacklist and selected-ID filters match on rendered mutant
-- sources, and mutator patterns, inline suppression, ignore lines and diff
-- lines filter the same population.  A configured filter counts as active
-- even when its file or list is empty, because a changed candidate
-- population still changes what the filter would see.
operatorSamplingEligible :: Opts -> [(Int, [String])] -> Bool
operatorSamplingEligible opts annotations =
    not (optCoverage opts)
        && null (optTix opts)
        && null (optDisable opts)
        && null (optEnable opts)
        && null (optIgnoreLines opts)
        && not (optGitDiffLines opts)
        && isNothing (optBaseline opts)
        && isNothing (optBlacklist opts)
        && isNothing (optRunMutantId opts)
        && null annotations

-- | Match a user-supplied pattern against a mutator name.
-- Trailing '*' acts as a prefix wildcard: "other:*" matches "other:remove-not".
matchesPat :: String -> String -> Bool
matchesPat pat name = case reverse pat of
  ('*' : revPrefix) -> reverse revPrefix `isPrefixOf` name
  _                 -> pat == name

-- | Apply --enable / --disable filters to a list of mutants.
applyDisableEnable :: [String] -> [String] -> [Mutant] -> [Mutant]
applyDisableEnable disable enable ms
  | not (null enable)  = filter (\m -> any (\p -> matchesPat p (muName m)) enable)  ms
  | not (null disable) = filter (\m -> not $ any (\p -> matchesPat p (muName m)) disable) ms
  | otherwise          = ms
  where muName = showMuVar . _mtype

-- | Parse inline @-- mucheck: disable-next-line [mutators]@ annotations.
-- Returns a list of (1-based comment line, suppressed mutator names).
-- An empty name list means suppress all mutators.
parseAnnotations :: String -> [(Int, [String])]
parseAnnotations src = concatMap check (zip [1..] (lines src))
  where
    check (n, line) =
      let trimmed = dropWhile isSpace line
      in case stripPrefix "-- mucheck: disable-next-line" trimmed of
           Nothing   -> []
           Just rest ->
             let names = case dropWhile isSpace rest of
                           "" -> []
                           s  -> splitOn ',' s
             in [(n, names)]

-- | Index annotations by the line they suppress. Line numbers start at 1.
-- True means every mutator on that line is suppressed.
indexAnnotations :: [(Int, [String])] -> IntMap.IntMap (Bool, Set.Set String)
indexAnnotations = IntMap.fromListWith merge . map toEntry
  where
    toEntry (annLine, names) =
      ( annLine + 1
      , if null names then (True, Set.empty) else (False, Set.fromList names)
      )
    merge (a1, n1) (a2, n2) = (a1 || a2, Set.union n1 n2)

-- | Filter out mutants suppressed by inline annotations.
applyAnnotations :: [(Int, [String])] -> [Mutant] -> [Mutant]
applyAnnotations [] ms = ms
applyAnnotations anns ms = filter (not . isSuppressed) ms
  where
    idx = indexAnnotations anns
    isSuppressed m =
      case IntMap.lookup (spanStartLine (_mspan m)) idx of
        Nothing -> False
        Just (suppressAll, names) ->
          suppressAll || Set.member (showMuVar (_mtype m)) names

-- | Load a baseline file and filter out mutants whose hash appears in it.
applyBaseline :: Maybe FilePath -> [Mutant] -> IO [Mutant]
applyBaseline Nothing ms = return ms
applyBaseline path ms =
  fmap uncacheMutantIds (applyBaselineCached path (cacheMutantIds ms))

-- | 'applyBaseline' on mutants that already have identities.
applyBaselineCached :: Maybe FilePath -> [(Mutant, String)] -> IO [(Mutant, String)]
applyBaselineCached Nothing ms = return ms
applyBaselineCached (Just path) ms = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left e -> do
      hPutStrLn stderr $ "Warning: could not read baseline file: " ++ show e
      return ms
    Right contents ->
      return $ filterCachedIds (`Set.notMember` indexIds (lines contents)) ms

-- | Load a blacklist file and filter out mutants whose hash appears in it.
applyBlacklist :: Maybe FilePath -> [Mutant] -> IO [Mutant]
applyBlacklist Nothing ms = return ms
applyBlacklist path ms =
  fmap uncacheMutantIds (applyBlacklistCached path (cacheMutantIds ms))

-- | 'applyBlacklist' on mutants that already have identities.
applyBlacklistCached :: Maybe FilePath -> [(Mutant, String)] -> IO [(Mutant, String)]
applyBlacklistCached Nothing ms = return ms
applyBlacklistCached (Just path) ms = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left e -> do
      hPutStrLn stderr $ "Warning: could not read blacklist file: " ++ show e
      return ms
    Right contents ->
      return $ filterCachedIds (`Set.notMember` indexIds (lines contents)) ms

-- | Return True if --git-diff-base is not set, or if the file appears in the diff.
checkGitDiff :: FilePath -> Maybe String -> IO Bool
checkGitDiff _ Nothing = return True
checkGitDiff file (Just ref) = do
  result <- try (readProcess "git" ["diff", "--name-only", ref] "") :: IO (Either IOException String)
  case result of
    Left _       -> return True
    Right output ->
      let changed = lines output
      in  return $ any (\c -> file == c || isSuffixOf c file || isSuffixOf file c) changed

-- | If --git-diff-lines is active (requires --git-diff-base), filter mutants
-- to those whose start line falls within lines changed relative to the base ref.
applyDiffLines :: FilePath -> Maybe String -> Bool -> [Mutant] -> IO [Mutant]
applyDiffLines _ Nothing _ ms = return ms
applyDiffLines _ _ False ms = return ms
applyDiffLines file ref flag ms =
  fmap uncacheMutantIds (applyDiffLinesCached file ref flag (cacheMutantIds ms))

-- | 'applyDiffLines' on mutants that already have identities.
applyDiffLinesCached :: FilePath -> Maybe String -> Bool -> [(Mutant, String)] -> IO [(Mutant, String)]
applyDiffLinesCached _    Nothing  _     ms = return ms
applyDiffLinesCached _    _        False ms = return ms
applyDiffLinesCached file (Just ref) True ms = do
  result <- try (readProcess "git" ["diff", "--unified=0", ref, "--", file] "") :: IO (Either IOException String)
  case result of
    Left _       -> return ms
    Right output ->
      let changed = indexChangedLines (parseDiffChangedLines output)
      in  return $ filter (\(m, _) -> spanStartLine (_mspan m) `IntSet.member` changed) ms

-- | Parse unified diff output (e.g. `git diff --unified=0` or `unifiedDiff` with context)
-- and return all changed line numbers in the new file.
parseDiffChangedLines :: String -> [Int]
parseDiffChangedLines = parseDiff . lines
  where
    parseDiff [] = []
    parseDiff (l:ls) = case parseHunkHeader l of
      Just (start, count) ->
        let (body, rest) = break isHunkHeader ls
            changed = parseHunkBody start body
            result = if null body && count > 0 then [start .. start + count - 1] else changed
        in result ++ parseDiff rest
      Nothing -> parseDiff ls

    isHunkHeader line = "@@ " `isPrefixOf` line

    parseHunkHeader line = case stripPrefix "@@ " line of
      Nothing   -> Nothing
      Just rest ->
        let plusPart = dropWhile (/= '+') rest
        in  case stripPrefix "+" plusPart of
              Nothing -> Nothing
              Just s  ->
                let (startStr, afterStart) = break (\c -> c == ',' || c == ' ') s
                in  case reads startStr of
                      [(start, "")] ->
                        let count = case afterStart of
                                      (',' : cs) -> case reads (takeWhile (/= ' ') cs) of
                                                      [(n, "")] -> n
                                                      _         -> 1
                                      _          -> 1
                        in  Just (start, count)
                      _ -> Nothing

    parseHunkBody _ [] = []
    parseHunkBody curLine (b:bs)
      | "+" `isPrefixOf` b && not ("+++" `isPrefixOf` b) =
          curLine : parseHunkBody (curLine + 1) bs
      | " " `isPrefixOf` b =
          parseHunkBody (curLine + 1) bs
      | "-" `isPrefixOf` b =
          parseHunkBody curLine bs
      | "\\" `isPrefixOf` b =
          parseHunkBody curLine bs
      | otherwise =
          parseHunkBody curLine bs

-- | Filter out mutants whose source start line contains any of the given substrings.
applyIgnoreLines :: String -> [String] -> [Mutant] -> [Mutant]
applyIgnoreLines _ [] ms = ms
applyIgnoreLines src patterns ms =
  uncacheMutantIds (applyIgnoreLinesCached src patterns (cacheMutantIds ms))

-- | 'applyIgnoreLines' on mutants that already have identities.
applyIgnoreLinesCached :: String -> [String] -> [(Mutant, String)] -> [(Mutant, String)]
applyIgnoreLinesCached _   []       ms = ms
applyIgnoreLinesCached src patterns ms = filter (not . isIgnored . fst) ms
  where
    table = indexSourceLines src
    isIgnored m =
      let ln = IntMap.findWithDefault "" (spanStartLine (_mspan m)) table
      in  any (`isInfixOf` ln) patterns

-- | Keep only the mutant matching the given stable ID; return all if Nothing.
applyRunMutantId :: Maybe String -> [Mutant] -> [Mutant]
applyRunMutantId Nothing ms = ms
applyRunMutantId mid ms =
  uncacheMutantIds (applyRunMutantIdCached mid (cacheMutantIds ms))

-- | 'applyRunMutantId' on mutants that already have identities.
applyRunMutantIdCached :: Maybe String -> [(Mutant, String)] -> [(Mutant, String)]
applyRunMutantIdCached Nothing    ms = ms
applyRunMutantIdCached (Just mid) ms = filterCachedIds (== mid) ms

-- | Index baseline or blacklist IDs. Skip empty lines.
indexIds :: [String] -> Set.Set String
indexIds = Set.fromList . filter (not . null)

-- | Index changed line numbers.
indexChangedLines :: [Int] -> IntSet.IntSet
indexChangedLines = IntSet.fromList

-- | Index source lines. Line numbers start at 1.
indexSourceLines :: String -> IntMap.IntMap String
indexSourceLines src = IntMap.fromList $ zip [1..] (lines src)

-- | Hash each mutant from its current source.
cacheMutantIds :: [Mutant] -> [(Mutant, String)]
cacheMutantIds ms = [(m, hash (_mutant m)) | m <- ms]

uncacheMutantIds :: [(Mutant, String)] -> [Mutant]
uncacheMutantIds = map fst

filterCachedIds :: (String -> Bool) -> [(Mutant, String)] -> [(Mutant, String)]
filterCachedIds keep = filter (keep . snd)
