{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Exception (IOException, try)
import Control.Monad (unless, when, forM_)
import Data.Char (isSpace)
import Data.List (group, isPrefixOf, nub, sort, sortBy, stripPrefix)
import Data.Ord (comparing, Down(..))
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import Test.MuCheck (sampler)
import Test.MuCheck.AnalysisSummary (MAnalysisSummary(..))
import Test.MuCheck.Config (MuVar(..), defaultConfig)
import Test.MuCheck.Interpreter (MutantSummary(..), evalTest, evaluateMutants)
import Test.MuCheck.Mutation (genMutants, genMutantsForSrc, getAllTests)
import Test.MuCheck.TestAdapter (InterpreterOutput(..), Mutant(..), Summarizable(..), TRun(..))
import Test.MuCheck.TestAdapter.AssertCheckAdapter
import Test.MuCheck.Tix (spanStartLine)
import Test.MuCheck.Utils.Print
import Test.MuCheck.Utils.Common (hash)


data Opts = Opts
  { optFile         :: FilePath
  , optTix          :: FilePath
  , optDryRun       :: Bool
  , optNoop         :: Bool
  , optFailOnEscape :: Bool
  , optMinMsi       :: Maybe Int
  , optMinCoveredMsi :: Maybe Int
  , optDisable      :: [String]
  , optEnable       :: [String]
  , optConfig       :: Maybe FilePath
  , optQuiet        :: Bool
  , optVerbose      :: Bool
  , optDebug        :: Bool
  , optNoDiffs      :: Bool
  , optIgnoreMsiNoMutations :: Bool
  , optOutputStatuses :: Maybe String
  , optTimeout      :: Maybe Int
  , optLoggerJson   :: Maybe FilePath
  , optBaseline     :: Maybe FilePath
  , optUpdateBaseline :: Maybe FilePath
  , optBlacklist    :: Maybe FilePath
  , optRunMutantId  :: Maybe String
  }

defaultOpts :: Opts
defaultOpts = Opts
  { optFile         = ""
  , optTix          = ""
  , optDryRun       = False
  , optNoop         = False
  , optFailOnEscape = False
  , optMinMsi       = Nothing
  , optMinCoveredMsi = Nothing
  , optDisable      = []
  , optEnable       = []
  , optConfig       = Nothing
  , optQuiet        = False
  , optVerbose      = False
  , optDebug        = False
  , optNoDiffs      = False
  , optIgnoreMsiNoMutations = False
  , optOutputStatuses = Nothing
  , optTimeout      = Nothing
  , optLoggerJson   = Nothing
  , optBaseline     = Nothing
  , optUpdateBaseline = Nothing
  , optBlacklist    = Nothing
  , optRunMutantId  = Nothing
  }

parseOpts :: [String] -> Either String Opts
parseOpts = go defaultOpts
  where
    go opts ("--dry-run"         : rest) = go opts { optDryRun       = True } rest
    go opts ("--noop"            : rest) = go opts { optNoop         = True } rest
    go opts ("--fail-on-escaped" : rest) = go opts { optFailOnEscape = True } rest
    go _    ("--min-msi"         : [])   = Left "--min-msi requires an integer argument"
    go opts ("--min-msi" : n     : rest) =
      case reads n of
        [(i, "")] -> go opts { optMinMsi = Just i } rest
        _         -> Left $ "--min-msi requires an integer argument, got: " ++ n
    go _    ("--min-covered-msi" : [])   = Left "--min-covered-msi requires an integer argument"
    go opts ("--min-covered-msi" : n : rest) =
      case reads n of
        [(i, "")] -> go opts { optMinCoveredMsi = Just i } rest
        _         -> Left $ "--min-covered-msi requires an integer argument, got: " ++ n
    go _    ("-tix"              : [])   = Left "-tix requires a file path argument"
    go opts ("-tix" : tix        : rest) = go opts { optTix = tix } rest
    go _    ("--disable"         : [])   = Left "--disable requires a name argument"
    go _    ("--disable" : "*"   : _)    = Left "--disable: bare '*' not allowed; use a prefix like 'functions/*'"
    go opts ("--disable" : n     : rest) = go opts { optDisable = n : optDisable opts } rest
    go _    ("--enable"          : [])   = Left "--enable requires a name argument"
    go _    ("--enable"  : "*"   : _)    = Left "--enable: bare '*' not allowed; use a prefix like 'functions/*'"
    go opts ("--enable"  : n     : rest) = go opts { optEnable = n : optEnable opts } rest
    go _    ("--config"          : [])   = Left "--config requires a file path argument"
    go opts ("--config"  : file  : rest) = go opts { optConfig = Just file } rest
    go opts ("--quiet"           : rest) = go opts { optQuiet = True } rest
    go opts ("--verbose"         : rest) = go opts { optVerbose = True } rest
    go opts ("--debug"           : rest) = go opts { optDebug = True } rest
    go opts ("--no-diffs"        : rest) = go opts { optNoDiffs = True } rest
    go opts ("--ignore-msi-with-no-mutations" : rest) = go opts { optIgnoreMsiNoMutations = True } rest
    go _    ("--output-statuses" : [])   = Left "--output-statuses requires a string argument"
    go opts ("--output-statuses" : chars : rest) = go opts { optOutputStatuses = Just chars } rest
    go _    ("--timeout"         : [])   = Left "--timeout requires an integer argument"
    go opts ("--timeout" : n     : rest) =
      case reads n of
        [(i, "")] -> go opts { optTimeout = Just i } rest
        _         -> Left $ "--timeout requires an integer argument, got: " ++ n
    go _    ("--logger-json"     : [])   = Left "--logger-json requires a file path argument"
    go opts ("--logger-json" : f : rest) = go opts { optLoggerJson = Just f } rest
    go _    ("--baseline"        : [])   = Left "--baseline requires a file path argument"
    go opts ("--baseline" : f    : rest) = go opts { optBaseline = Just f } rest
    go _    ("--update-baseline" : [])   = Left "--update-baseline requires a file path argument"
    go opts ("--update-baseline" : f : rest) = go opts { optUpdateBaseline = Just f } rest
    go _    ("--blacklist"       : [])   = Left "--blacklist requires a file path argument"
    go opts ("--blacklist" : f   : rest) = go opts { optBlacklist = Just f } rest
    go _    ("--run-mutant-id"   : [])   = Left "--run-mutant-id requires an ID argument"
    go opts ("--run-mutant-id" : i : rest) = go opts { optRunMutantId = Just i } rest
    go _    (arg@('-' : _)       : _)    = Left $ "Unknown flag: " ++ arg
    go opts (file                : _)    = Right opts { optFile = file }
    go _    []                           = Left "Need a file argument"

-- | Match a user-supplied pattern against a mutator name.
-- Trailing '*' acts as a prefix wildcard: "other:*" matches "other:remove-not".
matchesPat :: String -> String -> Bool
matchesPat pat name = case reverse pat of
  ('*' : revPrefix) -> reverse revPrefix `isPrefixOf` name
  _                 -> pat == name

-- | Apply --enable / --disable filters to a list of sampled mutants.
applyDisableEnable :: [String] -> [String] -> [Mutant] -> [Mutant]
applyDisableEnable disable enable ms
  | not (null enable)  = filter (\m -> any (\p -> matchesPat p (muName m)) enable)  ms
  | not (null disable) = filter (\m -> not $ any (\p -> matchesPat p (muName m)) disable) ms
  | otherwise          = ms
  where muName = showMuVar . _mtype

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-h" : _) -> help
    _          -> case parseOpts args of
      Left err   -> do putStrLn $ "Error: " ++ err; exitWith (ExitFailure 2)
      Right opts -> runOpts opts

runOpts :: Opts -> IO ()
runOpts opts
  | optDryRun opts = dryRun (optFile opts)
  | otherwise      = do
      when (optNoop opts) $ noopCheck (optFile opts)
      origSrc <- readFile (optFile opts)
      let modFile  = toRun (optFile opts) :: AssertCheckRun
          anns     = parseAnnotations origSrc
      (len, mutants) <- genMutants (getName modFile) (optTix opts)
      smutants        <- sampler defaultConfig mutants
      let filtered0 = applyDisableEnable (optDisable opts) (optEnable opts) smutants
          filtered1 = applyAnnotations anns filtered0
      filtered2 <- applyBaseline  (optBaseline opts)  filtered1
      filtered3 <- applyBlacklist (optBlacklist opts) filtered2
      let finalMutants = applyRunMutantId (optRunMutantId opts) filtered3
          tests        = map (genTest modFile)
      testNames <- getAllTests (getName modFile)
      let timeoutUs = fmap (* 1000000) (optTimeout opts)
      (fsum', tsum) <- evaluateMutants timeoutUs modFile finalMutants (tests testNames)
      let msum = case len of
                   -1 -> fsum' { _maCoveredNumMutants = -1 }
                   _  -> fsum' { _maCoveredNumMutants = length mutants }
      printMutantDetails opts origSrc tsum
      unless (isSingleMutantMode opts) $ do
        print msum
        printMutatorBreakdown opts tsum
        writeJsonLogger opts msum
        writeUpdateBaseline opts tsum
        applyExitPolicy opts msum

-- | True when --run-mutant-id is set (single-mutant mode skips aggregate output).
isSingleMutantMode :: Opts -> Bool
isSingleMutantMode = maybe False (const True) . optRunMutantId

-- | Keep only the mutant matching the given stable ID; return all if Nothing.
applyRunMutantId :: Maybe String -> [Mutant] -> [Mutant]
applyRunMutantId Nothing   ms = ms
applyRunMutantId (Just mid) ms = filter (\m -> hash (_mutant m) == mid) ms

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

-- | Split a string on a delimiter character, trimming whitespace from each part.
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s  = map (dropWhile isSpace . reverse . dropWhile isSpace . reverse) $ go s ""
  where
    go []     acc = [acc]
    go (x:xs) acc
      | x == c    = acc : go xs ""
      | otherwise = go xs (acc ++ [x])

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

-- | Write surviving mutant IDs to the update-baseline file.
writeUpdateBaseline :: Opts -> [MutantSummary] -> IO ()
writeUpdateBaseline opts tsum = case optUpdateBaseline opts of
  Nothing   -> return ()
  Just path -> do
    let aliveIds = [hash (_mutant m) | MSumAlive m _ <- tsum]
    writeFile path (unlines aliveIds)

-- | Write a compact JSON summary to the logger-json file.
writeJsonLogger :: Opts -> MAnalysisSummary -> IO ()
writeJsonLogger opts msum = case optLoggerJson opts of
  Nothing   -> return ()
  Just path -> do
    let MAnalysisSummary{..} = msum
        noerrors = _maNumMutants - _maErrors
        msiVal :: Double
        msiVal = if noerrors > 0
                 then fromIntegral _maKilled / fromIntegral noerrors
                 else 0.0
        covNoerrors = if _maCoveredNumMutants > 0
                      then _maCoveredNumMutants - _maErrors
                      else noerrors
        covMsiVal :: Double
        covMsiVal = if covNoerrors > 0
                    then fromIntegral _maKilled / fromIntegral covNoerrors
                    else 0.0
        covMsiField = if _maCoveredNumMutants > 0
                      then show covMsiVal
                      else "null"
        json = unlines
          [ "{"
          , "  \"total\": " ++ show _maNumMutants ++ ","
          , "  \"killed\": " ++ show _maKilled ++ ","
          , "  \"alive\": " ++ show _maAlive ++ ","
          , "  \"errors\": " ++ show _maErrors ++ ","
          , "  \"msi\": " ++ show msiVal ++ ","
          , "  \"covered_code_msi\": " ++ covMsiField
          , "}"
          ]
    writeFile path json

noopCheck :: FilePath -> IO ()
noopCheck file = do
  tests <- getAllTests file
  unless (null tests) $ do
    let testStrs = map (genTest (toRun file :: AssertCheckRun)) tests
        logF     = ".mucheck-noop.log"
        runTest :: String -> IO (InterpreterOutput AssertCheckSummary)
        runTest  = evalTest Nothing file logF
    results <- mapM runTest testStrs
    let pass = all (\r -> case _io r of { Right out -> isSuccess out; Left _ -> False }) results
    unless pass $ do
      putStrLn "Pre-flight check failed: test suite does not pass on unmodified source"
      exitWith (ExitFailure 3)

applyExitPolicy :: Opts -> MAnalysisSummary -> IO ()
applyExitPolicy opts msum = do
  let noerrors = _maNumMutants msum - _maErrors msum
      msi | noerrors > 0 = _maKilled msum * 100 `div` noerrors
          | otherwise    = 0
      
      -- Covered MSI is calculated using the covered mutants as the baseline.
      -- Since _maNumMutants is bounded by _maCoveredNumMutants if coverage is used,
      -- we can use the same `noerrors` if it's based on _maCoveredNumMutants, but 
      -- effectively the mutants tested ARE the covered ones when a tix file is provided.
      -- If -tix is not provided, _maCoveredNumMutants is -1.
      coveredNoerrors = if _maCoveredNumMutants msum > 0 
                        then _maCoveredNumMutants msum - _maErrors msum
                        else noerrors
      coveredMsi | coveredNoerrors > 0 = _maKilled msum * 100 `div` coveredNoerrors
                 | otherwise           = 0
  
  if optIgnoreMsiNoMutations opts && _maNumMutants msum == 0 then return ()
  else do
    case optMinMsi opts of
      Just threshold | msi < threshold -> do
        putStrLn $ "MSI " ++ show msi ++ "% is below threshold " ++ show threshold ++ "%"
        exitWith (ExitFailure 5)
      _ -> return ()
      
    case optMinCoveredMsi opts of
      Just threshold | coveredMsi < threshold -> do
        putStrLn $ "Covered-MSI " ++ show coveredMsi ++ "% is below threshold " ++ show threshold ++ "%"
        exitWith (ExitFailure 5)
      _ -> return ()
  
  when (optFailOnEscape opts && _maAlive msum > 0) $ do
    putStrLn $ show (_maAlive msum) ++ " mutant(s) survived; exiting with failure"
    exitWith (ExitFailure 4)

printMutatorBreakdown :: Opts -> [MutantSummary] -> IO ()
printMutatorBreakdown _ [] = return ()
printMutatorBreakdown _opts sums = do
  let mutOf (MSumError m _ _) = m
      mutOf (MSumAlive m _)   = m
      mutOf (MSumKilled m _)  = m
      mutOf (MSumOther m _)   = m
      isKilled (MSumKilled _ _)  = True; isKilled _ = False
      isAlive  (MSumAlive  _ _)  = True; isAlive  _ = False
      isErr    (MSumError _ _ _) = True; isErr    _ = False
      mutype   = showMuVar . _mtype . mutOf
      types    = sort . nub $ map mutype sums
      row t    = let ts = filter ((== t) . mutype) sums
                     k  = length $ filter isKilled ts
                     a  = length $ filter isAlive  ts
                     e  = length $ filter isErr    ts
                 in (t, k, a, e)
      rows     = map row types
      colW     = max 8 $ maximum $ map (\(t,_,_,_) -> length t) rows
      pad s    = s ++ replicate (colW - length s + 2) ' '
      sep      = replicate (colW + 32) '-'
      fmtN n   = replicate (max 0 (6 - length (show n))) ' ' ++ show n
  putStrLn ""
  putStrLn $ "  " ++ pad "Mutator" ++ "  Killed   Alive  Errors"
  putStrLn sep
  mapM_ (\(t,k,a,e) -> putStrLn $ "  " ++ pad t ++ "  " ++ fmtN k ++ "  " ++ fmtN a ++ "  " ++ fmtN e) rows
  putStrLn sep

printMutantDetails :: Opts -> String -> [MutantSummary] -> IO ()
printMutantDetails opts origSrc sums = do
  let filterStatuses s = case optOutputStatuses opts of
                           Nothing -> True
                           Just chars -> case s of
                             MSumKilled _ _ -> 'k' `elem` chars
                             MSumAlive _ _  -> 'a' `elem` chars
                             MSumError _ _ _ -> 'e' `elem` chars
                             MSumOther _ _  -> 'k' `elem` chars
      shouldShowQuiet s = not (optQuiet opts) || case s of { MSumAlive _ _ -> True; _ -> False }
      toShow = filter (\s -> filterStatuses s && shouldShowQuiet s) sums

  forM_ toShow $ \s -> do
    let (status, Mutant{..}, logS, mErr) = case s of
                                     MSumKilled mut l   -> ("KILLED", mut, l, Nothing)
                                     MSumAlive  mut l   -> ("ALIVE",  mut, l, Nothing)
                                     MSumError  mut e l -> ("ERROR",  mut, l, Just e)
                                     MSumOther  mut l   -> ("OTHER",  mut, l, Nothing)
    putStrLn $ ">>> Mutant " ++ hash _mutant ++ " [" ++ status ++ "] " ++ showMuVar _mtype
    unless (optNoDiffs opts) $
      let d = unifiedDiff origSrc _mutant
      in unless (null d) $ putStr d
    when (optVerbose opts) $ do
      putStrLn "--- Full source ---"
      putStrLn _mutant
      putStrLn "-------------------"
      mapM_ print logS
    when (optDebug opts) $
      case mErr of
        Just e  -> putStrLn $ "Error: " ++ e
        Nothing -> return ()
    putStrLn ""

dryRun :: FilePath -> IO ()
-- | Produce a compact unified diff between two source strings.
-- Shows only changed lines with up to 2 lines of context.
unifiedDiff :: String -> String -> String
unifiedDiff origSrc mutSrc
  | origSrc == mutSrc = ""
  | otherwise = concatMap renderHunk hunks
  where
    oLines   = lines origSrc
    mLines   = lines mutSrc
    maxLen   = max (length oLines) (length mLines)
    ctx      = 2
    getL i ls = if i <= length ls then ls !! (i - 1) else ""
    diffIdxs = [i | i <- [1..maxLen], getL i oLines /= getL i mLines]
    ctxSet   = nub $ sort $ concatMap (\i -> [max 1 (i-ctx)..min maxLen (i+ctx)]) diffIdxs
    hunks    = groupConsec ctxSet
    renderHunk hunk@(lo:_) =
      let hi  = hunk !! (length hunk - 1)
          hdr = "@@ -" ++ show lo ++ "," ++ show (hi - lo + 1) ++ " @@\n"
          rows = concat (concatMap renderRow hunk)
      in hdr ++ rows
    renderHunk [] = ""
    renderRow i
      | i `elem` diffIdxs =
          let oL = getL i oLines; mL = getL i mLines
          in  ["-" ++ oL ++ "\n" | not (null oL) || i <= length oLines]
           ++ ["+" ++ mL ++ "\n" | not (null mL) || i <= length mLines]
      | otherwise = [" " ++ getL i oLines ++ "\n"]

-- | Group a sorted list of ints into runs of consecutive integers.
groupConsec :: [Int] -> [[Int]]
groupConsec [] = []
groupConsec (x:xs) = go [x] x xs
  where
    go cur _    []     = [cur]
    go cur prev (y:ys)
      | y == prev + 1  = go (cur ++ [y]) y ys
      | otherwise      = cur : go [y] y ys

dryRun file = do
  src <- readFile file
  let mutants = genMutantsForSrc defaultConfig src
      byType  = [(v, length g) | g@(v:_) <- group . sort $ map _mtype mutants]
      byType' = sortBy (comparing (Down . snd)) byType
      colW    = max 7 $ maximum $ map (length . showMuVar . fst) byType'
      pad s   = s ++ replicate (colW - length s + 2) ' '
      sep     = replicate (colW + 10) '-'
      rows    = map (\(v, n) -> "  " ++ pad (showMuVar v) ++ show n) byType'
      total   = length mutants
  putStrLn $ "  " ++ pad "Mutator" ++ "Count"
  putStrLn sep
  mapM_ putStrLn rows
  putStrLn sep
  putStrLn $ "  " ++ pad "Total" ++ show total
  putStrLn "(upper bound; identical mutations are deduplicated before evaluation)"

showMuVar :: MuVar -> String
showMuVar MutatePatternMatch          = "pattern-match"
showMuVar MutateValues                = "literal-values"
showMuVar MutateFunctions             = "functions"
showMuVar MutateNegateIfElse          = "negate-if-else"
showMuVar MutateNegateGuards          = "negate-guards"
showMuVar (MutateOther "remove-not")  = "remove-not"
showMuVar (MutateOther "remove-negation") = "remove-negation"
showMuVar (MutateOther s)             = if null s then "other" else "other:" ++ s

help :: IO ()
help = putStrLn $ showAS
  [ "Usage: mucheck [FLAGS] FILE"
  , ""
  , "FLAGS:"
  , "  -h                        Print this help"
  , "  --dry-run                 Show mutation counts by type without evaluating"
  , "  --noop                    Verify tests pass on unmodified source first (exit 3 on failure)"
  , "  --fail-on-escaped         Exit with code 4 if any mutant survives"
  , "  --min-msi PCT             Exit with code 5 if MSI is below PCT percent"
  , "  --disable NAME            Skip mutants of the named type (repeatable)"
  , "  --enable  NAME            Run only mutants of the named type (repeatable)"
  , "  -tix FILE                 HPC coverage file for coverage-guided mutation"
  , "  --logger-json FILE        Write a compact JSON run summary to FILE"
  , "  --baseline FILE           Skip mutants whose ID appears in FILE from a previous run"
  , "  --update-baseline FILE    Write surviving mutant IDs to FILE after the run"
  , "  --blacklist FILE          Suppress mutations whose ID appears in FILE (false-positive exclusions)"
  , "  --run-mutant-id ID        Evaluate only the mutant with the given stable ID; no aggregate summary"
  , ""
  , "MUTATOR NAMES (for --disable / --enable):"
  , "  pattern-match             Function pattern-match permutation and removal"
  , "  literal-values            Integer, float, char, string, and boolean literals"
  , "  functions                 Operator and function substitution"
  , "  negate-if-else            Swap if-then and if-else branches"
  , "  negate-guards             Wrap guard conditions in 'not'"
  , "  remove-not                Strip 'not' from negated sub-expressions"
  , "  remove-negation           Strip 'negate' and prefix '-' from expressions"
  , "  Trailing '*' is a prefix wildcard, e.g. 'other:*'"
  , ""
  , "EXIT CODES:"
  , "  0  Tests ran; no quality gate triggered"
  , "  2  Bad arguments"
  , "  3  Pre-flight failure (--noop: tests fail on original source)"
  , "  4  Escaped mutants (--fail-on-escaped)"
  , "  5  MSI below threshold (--min-msi)"
  , ""
  , "E.g.:"
  , "  mucheck [--dry-run] [-tix file.tix] Examples/AssertCheckTest.hs"
  ]
