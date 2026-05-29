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
  ) where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, stripPrefix)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)

import App.Opts (splitOn)
import Test.Mutaskell.Config (showMuVar)
import Test.Mutaskell.TestAdapter (Mutant(..))
import Test.Mutaskell.Tix (spanStartLine)
import Test.Mutaskell.Utils.Common (hash)

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

-- | Filter out mutants suppressed by inline annotations.
applyAnnotations :: [(Int, [String])] -> [Mutant] -> [Mutant]
applyAnnotations [] ms = ms
applyAnnotations anns ms = filter (not . isSuppressed) ms
  where
    isSuppressed m =
      let sl      = spanStartLine (_mspan m)
          mName   = showMuVar (_mtype m)
      in any (\(annLine, names) ->
                sl == annLine + 1 &&
                (null names || mName `elem` names)
             ) anns

-- | Load a baseline file and filter out mutants whose hash appears in it.
applyBaseline :: Maybe FilePath -> [Mutant] -> IO [Mutant]
applyBaseline Nothing ms = return ms
applyBaseline (Just path) ms = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left e -> do
      hPutStrLn stderr $ "Warning: could not read baseline file: " ++ show e
      return ms
    Right contents -> do
      let ids = filter (not . null) (lines contents)
      return $ filter (\m -> hash (_mutant m) `notElem` ids) ms

-- | Load a blacklist file and filter out mutants whose hash appears in it.
applyBlacklist :: Maybe FilePath -> [Mutant] -> IO [Mutant]
applyBlacklist Nothing ms = return ms
applyBlacklist (Just path) ms = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left e -> do
      hPutStrLn stderr $ "Warning: could not read blacklist file: " ++ show e
      return ms
    Right contents -> do
      let ids = filter (not . null) (lines contents)
      return $ filter (\m -> hash (_mutant m) `notElem` ids) ms

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
applyDiffLines _    Nothing  _     ms = return ms
applyDiffLines _    _        False ms = return ms
applyDiffLines file (Just ref) True ms = do
  result <- try (readProcess "git" ["diff", "--unified=0", ref, "--", file] "") :: IO (Either IOException String)
  case result of
    Left _       -> return ms
    Right output ->
      let changedLines = parseDiffChangedLines output
          inChanged m  = spanStartLine (_mspan m) `elem` changedLines
      in  return $ filter inChanged ms

-- | Parse `git diff --unified=0` output and return all changed line numbers in the new file.
parseDiffChangedLines :: String -> [Int]
parseDiffChangedLines = concatMap parseHunk . lines
  where
    parseHunk line = case stripPrefix "@@ " line of
      Nothing   -> []
      Just rest ->
        let plusPart = dropWhile (/= '+') rest
        in  case stripPrefix "+" plusPart of
              Nothing -> []
              Just s  ->
                let (startStr, afterStart) = break (\c -> c == ',' || c == ' ') s
                in  case reads startStr of
                      [(start, "")] ->
                        let count = case afterStart of
                                      (',' : cs) -> case reads (takeWhile (/= ' ') cs) of
                                                      [(n, "")] -> n
                                                      _         -> 1
                                      _          -> 1
                        in  [start .. start + count - 1]
                      _ -> []

-- | Filter out mutants whose source start line contains any of the given substrings.
applyIgnoreLines :: String -> [String] -> [Mutant] -> [Mutant]
applyIgnoreLines _   []       ms = ms
applyIgnoreLines src patterns ms = filter (not . isIgnored) ms
  where
    srcLines = lines src
    isIgnored m =
      let sl = spanStartLine (_mspan m)
          ln = if sl >= 1 && sl <= length srcLines then srcLines !! (sl - 1) else ""
      in  any (`isInfixOf` ln) patterns

-- | Keep only the mutant matching the given stable ID; return all if Nothing.
applyRunMutantId :: Maybe String -> [Mutant] -> [Mutant]
applyRunMutantId Nothing   ms = ms
applyRunMutantId (Just mid) ms = filter (\m -> hash (_mutant m) == mid) ms
