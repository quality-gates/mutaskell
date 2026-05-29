{-# LANGUAGE RankNTypes #-}

-- | SYB (Scrap Your Boilerplate) utilities for AST traversal
module Test.Muskell.Utils.Syb (relevantOps, once) where

import Control.Monad (MonadPlus, mplus, mzero)
import Data.Generics (Data, GenericM, gmapMo, mkQ)

import GHC.Hs (HsDecl (..), LHsDecl, GhcPs)
import GHC.Types.SrcLoc (GenLocated (..))

import Test.Muskell.Config (MuVar)
import Test.Muskell.MuOp (MuOp, same)

-- | Returns @True@ for declaration forms that should never be traversed into
-- for mutations: type\/class\/instance heads, type signatures, standalone
-- @deriving@, annotation pragmas, and foreign declarations.
--
-- We check both the bare @'HsDecl' 'GhcPs'@ (reached when syb peels the
-- 'GenLocated' wrapper) and the located @'LHsDecl' 'GhcPs'@ (the top-level
-- entry in a module\'s declaration list).
isSkippedDecl :: Data a => a -> Bool
isSkippedDecl x = mkQ False checkBare x || mkQ False checkLocated x
  where
    checkBare :: HsDecl GhcPs -> Bool
    checkBare TyClD{} = True   -- class / data / type / family decls
    checkBare InstD{}  = True  -- instance / deriving-instance decls
    checkBare SigD{}   = True  -- type signatures
    checkBare DerivD{} = True  -- standalone deriving
    checkBare AnnD{}   = True  -- {-# ANN #-} pragmas (test annotations)
    checkBare ForD{}   = True  -- foreign import / export
    checkBare RuleD{}  = True  -- RULES pragmas
    checkBare _        = False

    checkLocated :: LHsDecl GhcPs -> Bool
    checkLocated (L _ d) = checkBare d

-- | Apply a mutating function on a piece of code exactly once at the first
-- matching site found in a depth-first traversal.  Skips sub-trees rooted at
-- structural or declarative nodes (see 'isSkippedDecl') to avoid generating
-- non-compilable mutants.
once :: MonadPlus m => GenericM m -> GenericM m
once f x = f x `mplus` (if isSkippedDecl x then mzero else gmapMo (once f) x)

-- | Filter out identity operations (where @before === after@ by 'ppr').
-- The applicability check is deferred to 'once' in 'mutate', which returns an
-- empty list for operators that do not match any AST node.
relevantOps :: (Data a) => a -> [(MuVar, MuOp)] -> [(MuVar, MuOp)]
relevantOps _ = filter (not . same . snd)
