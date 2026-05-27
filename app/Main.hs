module Main where

import Control.Monad (unless, when)
import Data.List (group, isPrefixOf, nub, sort, sortBy)
import Data.Ord (comparing, Down(..))
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)

import Test.MuCheck (sampler)
import Test.MuCheck.AnalysisSummary (MAnalysisSummary(..))
import Test.MuCheck.Config (MuVar(..), defaultConfig)
import Test.MuCheck.Interpreter (MutantSummary(..), evalTest, evaluateMutants)
import Test.MuCheck.Mutation (genMutants, genMutantsForSrc, getAllTests)
import Test.MuCheck.TestAdapter (InterpreterOutput(..), Mutant(..), Summarizable(..), TRun(..))
import Test.MuCheck.TestAdapter.AssertCheckAdapter
import Test.MuCheck.Utils.Print

data Opts = Opts
  { optFile         :: FilePath
  , optTix          :: FilePath
  , optDryRun       :: Bool
  , optNoop         :: Bool
  , optFailOnEscape :: Bool
  , optMinMsi       :: Maybe Int
  , optDisable      :: [String]
  , optEnable       :: [String]
  }

defaultOpts :: Opts
defaultOpts = Opts
  { optFile         = ""
  , optTix          = ""
  , optDryRun       = False
  , optNoop         = False
  , optFailOnEscape = False
  , optMinMsi       = Nothing
  , optDisable      = []
  , optEnable       = []
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
    go _    ("-tix"              : [])   = Left "-tix requires a file path argument"
    go opts ("-tix" : tix        : rest) = go opts { optTix = tix } rest
    go _    ("--disable"         : [])   = Left "--disable requires a name argument"
    go _    ("--disable" : "*"   : _)    = Left "--disable: bare '*' not allowed; use a prefix like 'functions/*'"
    go opts ("--disable" : n     : rest) = go opts { optDisable = n : optDisable opts } rest
    go _    ("--enable"          : [])   = Left "--enable requires a name argument"
    go _    ("--enable"  : "*"   : _)    = Left "--enable: bare '*' not allowed; use a prefix like 'functions/*'"
    go opts ("--enable"  : n     : rest) = go opts { optEnable = n : optEnable opts } rest
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
      let modFile = toRun (optFile opts) :: AssertCheckRun
      (len, mutants) <- genMutants (getName modFile) (optTix opts)
      smutants        <- sampler defaultConfig mutants
      let finalMutants = applyDisableEnable (optDisable opts) (optEnable opts) smutants
          tests        = map (genTest modFile)
      testNames <- getAllTests (getName modFile)
      (fsum', tsum) <- evaluateMutants modFile finalMutants (tests testNames)
      let msum = case len of
                   -1 -> fsum' { _maOriginalNumMutants = -1, _maCoveredNumMutants = -1 }
                   _  -> fsum' { _maOriginalNumMutants = len, _maCoveredNumMutants = length mutants }
      print msum
      printMutatorBreakdown tsum
      applyExitPolicy opts msum

noopCheck :: FilePath -> IO ()
noopCheck file = do
  tests <- getAllTests file
  unless (null tests) $ do
    let testStrs = map (genTest (toRun file :: AssertCheckRun)) tests
        logF     = ".mucheck-noop.log"
        runTest :: String -> IO (InterpreterOutput AssertCheckSummary)
        runTest  = evalTest file logF
    results <- mapM runTest testStrs
    let pass = all (\r -> case _io r of { Right out -> isSuccess out; Left _ -> False }) results
    unless pass $ do
      putStrLn "Pre-flight check failed: test suite does not pass on unmodified source"
      exitWith (ExitFailure 3)

applyExitPolicy :: Opts -> MAnalysisSummary -> IO ()
applyExitPolicy opts msum = do
  let noerrors = max (max (_maOriginalNumMutants msum) (_maCoveredNumMutants msum))
                     (_maNumMutants msum) - _maErrors msum
      msi | noerrors > 0 = _maKilled msum * 100 `div` noerrors
          | otherwise    = 0
  case optMinMsi opts of
    Just threshold | msi < threshold -> do
      putStrLn $ "MSI " ++ show msi ++ "% is below threshold " ++ show threshold ++ "%"
      exitWith (ExitFailure 5)
    _ -> return ()
  when (optFailOnEscape opts && _maAlive msum > 0) $ do
    putStrLn $ show (_maAlive msum) ++ " mutant(s) survived; exiting with failure"
    exitWith (ExitFailure 4)

printMutatorBreakdown :: [MutantSummary] -> IO ()
printMutatorBreakdown [] = return ()
printMutatorBreakdown sums = do
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

dryRun :: FilePath -> IO ()
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
  , "  -h                   Print this help"
  , "  --dry-run            Show mutation counts by type without evaluating"
  , "  --noop               Verify tests pass on unmodified source first (exit 3 on failure)"
  , "  --fail-on-escaped    Exit with code 4 if any mutant survives"
  , "  --min-msi PCT        Exit with code 5 if MSI is below PCT percent"
  , "  --disable NAME       Skip mutants of the named type (repeatable)"
  , "  --enable  NAME       Run only mutants of the named type (repeatable)"
  , "  -tix FILE            HPC coverage file for coverage-guided mutation"
  , ""
  , "MUTATOR NAMES (for --disable / --enable):"
  , "  pattern-match        Function pattern-match permutation and removal"
  , "  literal-values       Integer, float, char, string, and boolean literals"
  , "  functions            Operator and function substitution"
  , "  negate-if-else       Swap if-then and if-else branches"
  , "  negate-guards        Wrap guard conditions in 'not'"
  , "  remove-not           Strip 'not' from negated sub-expressions"
  , "  remove-negation      Strip 'negate' and prefix '-' from expressions"
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
