{-# LANGUAGE RankNTypes #-}
-- | SYB functions
module Test.MuCheck.Utils.Syb (relevantOps, once) where

import Data.Generics (Data, GenericM, gmapMo, mkQ)
import Test.MuCheck.MuOp (MuOp, same)
import Test.MuCheck.Config (MuVar)
import Control.Monad (MonadPlus, mplus, mzero)
import Language.Haskell.Exts (Decl(TypeSig, InstDecl, ClassDecl, TypeDecl, DataDecl, GDataDecl, TypeFamDecl, DataFamDecl, ClosedTypeFamDecl, TypeInsDecl, DataInsDecl, GDataInsDecl), SrcSpanInfo)

isSkippedDecl :: Data a => a -> Bool
isSkippedDecl = mkQ False checkDecl
  where
    checkDecl :: Decl SrcSpanInfo -> Bool
    checkDecl TypeSig{} = True
    checkDecl InstDecl{} = True
    checkDecl ClassDecl{} = True
    checkDecl TypeDecl{} = True
    checkDecl DataDecl{} = True
    checkDecl GDataDecl{} = True
    checkDecl TypeFamDecl{} = True
    checkDecl DataFamDecl{} = True
    checkDecl ClosedTypeFamDecl{} = True
    checkDecl TypeInsDecl{} = True
    checkDecl DataInsDecl{} = True
    checkDecl GDataInsDecl{} = True
    checkDecl _ = False

-- | apply a mutating function on a piece of code one at a time
-- like somewhere (from so)
once :: MonadPlus m => GenericM m -> GenericM m
once f x = f x `mplus` (if isSkippedDecl x then mzero else gmapMo (once f) x)

-- | Filter out identity ops (where source == target).
-- The applicability check (traversal) is deferred to 'once' in 'mutate',
-- which returns an empty list for ops that do not match any AST node.
-- This removes the duplicate AST traversal that the old relevance-probe pass performed.
relevantOps :: (Data a, Eq a) => a -> [(MuVar, MuOp)] -> [(MuVar, MuOp)]
relevantOps _ oplst = filter (not . same . snd) oplst

