{-# LANGUAGE RecordWildCards #-}

-- | MuCheck base module
module Test.Muskell (mucheck, sampler) where

import Test.Muskell.AnalysisSummary
import Test.Muskell.Config
import Test.Muskell.Interpreter (MutantSummary (..), evaluateMutants)
import Test.Muskell.Mutation
import Test.Muskell.TestAdapter
import Test.Muskell.Utils.Common

{- | Perform mutation analysis using any of the test frameworks that support
Summarizable (essentially, after running it on haskell, we should be able to
distinguish a successful run without failures from one with failures.)
E.g. using the mucheck-quickcheck adapter

> tFn :: Mutant -> TestStr -> InterpreterOutput QuickCheckSummary`
> tFn = testSummary
> mucheck tFn "Examples/QuickCheckTest.hs" ["quickCheckResult revProp"]
-}
mucheck ::
    (Show b, Summarizable b, TRun a b) =>
    -- | The module we are mutating
    a ->
    -- | The HPC <coverage>.tix file
    FilePath ->
    -- | Returns a tuple of full summary, and individual mutant results.
    IO (Either String (MAnalysisSummary, [MutantSummary]))
mucheck moduleFile tix = do
  -- get tix here.
  res <- genMutants (getName moduleFile) tix
  case res of
    Left err -> return $ Left err
    Right (len, mutants) -> do
      -- Should we do random sample on covering alone or on the full?
      smutants <- sampler defaultConfig mutants
      testRes <- getAllTests (getName moduleFile)
      case testRes of
        Left err -> return $ Left err
        Right tests -> do
          (fsum', msum) <- evaluateMutants 1 Nothing Nothing [] Nothing moduleFile smutants (map (genTest moduleFile) tests)
          -- set the original size of mutants. (We report the results based on original
          -- number of mutants, not just the covered ones.)
          let fsum = case len of
               -1 -> fsum' { _maCoveredNumMutants = -1 }
               _  -> fsum' { _maCoveredNumMutants = length mutants }
          return $ Right (fsum, msum)

{- | Wrapper around sampleF that returns correct sampling ratios according to
configuration passed. TODO: Actually use the sampling configuration.
-}
sampler ::
    -- | Configuration
    Config ->
    -- | The original list of mutation operators
    [Mutant] ->
    -- | Returns the sampled mutation operators
    IO [Mutant]
sampler config mv = do
    ms <-
        concat
            <$> mapM
                (getSampled config mv)
                [ MutatePatternMatch
                , MutateValues
                , MutateFunctions
                , MutateNegateIfElse
                , MutateNegateGuards
                , MutateOther []
                ]
    rSample (maxNumMutants config) ms

getSampled :: Config -> [Mutant] -> MuVar -> IO [Mutant]
getSampled config ms muvar = rSampleF (getSample muvar config) $ filter (mutantIs muvar) ms
  where
    mutantIs mvar Mutant{..} = mvar `similar` _mtype
