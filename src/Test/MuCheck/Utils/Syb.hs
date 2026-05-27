{-# LANGUAGE RankNTypes #-}

-- | SYB functions
module Test.MuCheck.Utils.Syb (relevantOps, once) where

import Control.Monad (MonadPlus, mplus, mzero)
import Data.Generics (Data, GenericM, Typeable, gmapMo, mkQ)
import Data.Maybe (isJust)
import Language.Haskell.Exts (Decl (ClassDecl, ClosedTypeFamDecl, DataDecl, DataFamDecl, DataInsDecl, GDataDecl, GDataInsDecl, InstDecl, TypeDecl, TypeFamDecl, TypeInsDecl, TypeSig), SrcSpanInfo)
import Test.MuCheck.Config (MuVar)
import Test.MuCheck.MuOp (MuOp, mkMpMuOp, same)

isSkippedDecl :: (Data a) => a -> Bool
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

{- | apply a mutating function on a piece of code one at a time
like somewhere (from so)
-}
once :: (MonadPlus m) => GenericM m -> GenericM m
once f x = f x `mplus` (if isSkippedDecl x then mzero else gmapMo (once f) x)

{- | The function `relevantOps` does two filters. For the first, it
removes spurious transformations like "Int 1 ~~> Int 1". Secondly, it
tries to apply the transformation to the given program on some element
if it does not succeed, then we discard that transformation.
-}
relevantOps :: (Data a, Eq a) => a -> [(MuVar, MuOp)] -> [(MuVar, MuOp)]
relevantOps m oplst = filter (relevantOp m) $ filter (not . same . snd) oplst
  where
    -- check if an operator can be applied to a program
    relevantOp m' (_v, op) = isJust $ once (mkMpMuOp op) m'
