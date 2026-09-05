{-# LANGUAGE RecordWildCards #-}

{- | The AnalysisSummary declares the mutation result datatype, and its
instances.
-}
module Test.Mutaskell.AnalysisSummary where

import Test.Mutaskell.Utils.Print

-- | Datatype to hold results of the entire run
data MAnalysisSummary = MAnalysisSummary {
    _maCoveredNumMutants::Int   -- ^ The number of mutants that was covered by test cases
  , _maNumMutants::Int     -- ^ The number of mutants tested (after sampling)
  , _maAlive::Int          -- ^ The number of mutants that were alive after the mutation run
  , _maKilled::Int         -- ^ The number of mutants that were killed
  , _maErrors::Int         -- ^ The number of mutants that produced interpreter errors
  , _maSkipped::Int        -- ^ The number of non-compilable mutants (WontCompile errors)
} deriving (Eq, Read)

instance Semigroup MAnalysisSummary where
  s1 <> s2 = MAnalysisSummary
    { _maCoveredNumMutants = case (_maCoveredNumMutants s1, _maCoveredNumMutants s2) of
        (-1, -1) -> -1
        (-1, c2) -> c2
        (c1, -1) -> c1
        (c1, c2) -> c1 + c2
    , _maNumMutants = _maNumMutants s1 + _maNumMutants s2
    , _maAlive      = _maAlive s1 + _maAlive s2
    , _maKilled     = _maKilled s1 + _maKilled s2
    , _maErrors     = _maErrors s1 + _maErrors s2
    , _maSkipped    = _maSkipped s1 + _maSkipped s2
    }

instance Monoid MAnalysisSummary where
  mempty = MAnalysisSummary (-1) 0 0 0 0 0

-- | Total mutant count acting as the basis for percentage calculations.
summaryTotal :: MAnalysisSummary -> Int
summaryTotal s = max (_maCoveredNumMutants s) (_maNumMutants s)

-- | Number of non-error mutants evaluated.
summaryNoErrors :: MAnalysisSummary -> Int
summaryNoErrors s = summaryTotal s - _maErrors s

-- | Mutation Score Indicator (MSI) as a percentage (0–100).
summaryMsi :: MAnalysisSummary -> Int
summaryMsi s
  | noerrs > 0 = _maKilled s * 100 `div` noerrs
  | otherwise  = 0
  where noerrs = summaryNoErrors s

-- | Covered mutant count excluding interpreter errors, when coverage is present.
summaryCoveredNoErrors :: MAnalysisSummary -> Maybe Int
summaryCoveredNoErrors s
  | _maCoveredNumMutants s > 0 = Just (_maCoveredNumMutants s - _maErrors s)
  | otherwise                  = Nothing

-- | Covered MSI percentage when coverage is present.
summaryCoveredMsi :: MAnalysisSummary -> Maybe Int
summaryCoveredMsi s = do
  cne <- summaryCoveredNoErrors s
  return $ if cne > 0 then _maKilled s * 100 `div` cne else 0

-- | MAnalysisSummary to tuple
maSummary :: MAnalysisSummary -> (Int, Int, Int, Int, Int, Int)
maSummary MAnalysisSummary{..} = (_maCoveredNumMutants, _maNumMutants, _maAlive, _maKilled, _maErrors, _maSkipped)

-- | The show instance for MAnalysisSummary
instance Show MAnalysisSummary where
  show s@MAnalysisSummary{..} =
    let mnum     = summaryTotal s
        noerrors = summaryNoErrors s
        msi      = summaryMsi s
        showx a  = if a == -1 then "not provided" else show a
    in showAS ["Mutation score (MSI): " ++ show msi ++ "%",
               "Total mutants: " ++ show mnum ++ " (basis for %)",
               "\tCovered: " ++  showx _maCoveredNumMutants,
               "\tSampled: " ++  show _maNumMutants,
               "\tSkipped (non-compilable): " ++ show _maSkipped,
               "\tErrors: " ++  show _maErrors ++ "  "++ _maErrors ./. mnum,
               "\tAlive: " ++  show _maAlive  ++ "/" ++ show noerrors,
               "\tKilled: " ++  show _maKilled ++ "/" ++ show noerrors ++ " " ++ _maKilled ./. noerrors]

