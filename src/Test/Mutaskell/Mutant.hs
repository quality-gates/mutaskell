-- | A rendered mutation and its origin.
module Test.Mutaskell.Mutant (Mutant (..), toMutant) where

import Test.Mutaskell.Config (MuVar)
import Test.Mutaskell.Tix (Span)

data Mutant = Mutant {_mutant :: String, _mtype :: MuVar, _mspan :: Span}
    deriving (Eq, Show, Read)

toMutant :: (MuVar, Span, String) -> Mutant
toMutant (m, s, str) = Mutant{_mutant = str, _mtype = m, _mspan = s}
