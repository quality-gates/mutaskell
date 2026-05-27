module Main where
import Data.List (group, sort, sortBy)
import Data.Ord (comparing, Down(..))
import System.Environment (getArgs)

import Test.MuCheck (mucheck)
import Test.MuCheck.Config (MuVar(..), defaultConfig)
import Test.MuCheck.Mutation (genMutantsForSrc)
import Test.MuCheck.TestAdapter (Mutant(..), TRun(..))
import Test.MuCheck.TestAdapter.AssertCheckAdapter
import Test.MuCheck.Utils.Print

main :: IO ()
main = do
  val <- getArgs
  case val of
    ("-h" : _ ) -> help
    ("--dry-run" : "-tix" : tix : file : _) -> dryRun file (Just tix)
    ("--dry-run" : file : _)                 -> dryRun file Nothing
    ("-tix" : tix: file: _ ) -> do (msum, _tsum) <- mucheck (toRun file :: AssertCheckRun) tix
                                   print msum
    (file : _args) -> do (msum, _tsum) <- mucheck (toRun file :: AssertCheckRun) []
                         print msum
    _ -> error "Need function file [args]\n\tUse -h to get help"

dryRun :: FilePath -> Maybe FilePath -> IO ()
dryRun file _tix = do
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
showMuVar MutatePatternMatch = "pattern-match"
showMuVar MutateValues       = "literal-values"
showMuVar MutateFunctions    = "functions"
showMuVar MutateNegateIfElse = "negate-if-else"
showMuVar MutateNegateGuards = "negate-guards"
showMuVar (MutateOther s)    = if null s then "other" else "other:" ++ s

help :: IO ()
help = putStrLn $ "mucheck function file [args]\n" ++ showAS ["E.g:",
       " mucheck [--dry-run] [-tix <file.tix>] Examples/AssertCheckTest.hs",""]
