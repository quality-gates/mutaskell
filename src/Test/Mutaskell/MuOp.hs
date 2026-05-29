{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Mutation operators
module Test.Mutaskell.MuOp (
    MuOp,
    Mutable (..),
    (==>*),
    (*==>*),
    (~~>),
    mkMpMuOp,
    same,
    Module_,
    Expr_,
    Decl_,
    Stmt_,
    Alt_,
    Rhs_,
    GuardedRhs_,   -- exported for use as a helper type in Mutation.hs
    MuNode,
    CanTransfer (..),
    getSpan,
) where

import Control.Monad (MonadPlus, mzero)
import qualified Data.Generics as G

import GHC.Hs
import GHC.Parser.Annotation ()
import GHC.Types.SrcLoc
    ( GenLocated (..), SrcSpan (..)
    , srcLocLine, srcLocCol
    , realSrcSpanStart, realSrcSpanEnd
    )
import GHC.Utils.Outputable (Outputable, showSDocUnsafe, ppr)
import Language.Haskell.GHC.ExactPrint.Transform (transferEntryDP)

-- ---------------------------------------------------------------------------
-- Type aliases

-- | The bare parsed module (inside Located; used as the mutation target)
type Module_ = HsModule GhcPs

-- | A located Haskell expression
type Expr_ = LHsExpr GhcPs

-- | A located Haskell declaration
type Decl_ = LHsDecl GhcPs

-- | A located statement in a @do@-block
type Stmt_ = ExprLStmt GhcPs

-- | A located case alternative / function match
type Alt_ = LMatch GhcPs (LHsExpr GhcPs)

-- | A guarded right-hand side group (not located; sits inside a @Match@)
type Rhs_ = GRHSs GhcPs (LHsExpr GhcPs)

-- | A single located guard (@GRHS@)
type GuardedRhs_ = LGRHS GhcPs (LHsExpr GhcPs)

-- ---------------------------------------------------------------------------
-- Node identity and annotation repair

{- | Typeclass that captures the ability to transfer an entry delta from one
located node to another.  All 'MuOp' node types are @GenLocated (EpAnn t) a@
for some @'G.Typeable' t@, which makes the transfer possible via
'transferEntryDP'.
-}
class CanTransfer a where
    -- | Copy the leading-whitespace delta from @z@ (original, in the AST) to
    -- @y@ (synthetic replacement), so that 'exactPrint' preserves layout.
    transferEntry :: a -> a -> a

instance G.Typeable t => CanTransfer (GenLocated (EpAnn t) a) where
    transferEntry = transferEntryDP

{- | Constraint alias for everything a node type needs to participate in
mutation: generics traversal (@Typeable@), source-location access (@HasLoc@),
annotation repair (@CanTransfer@), and pretty-printing (@Outputable@).
-}
type MuNode a = (G.Typeable a, HasLoc a, CanTransfer a, Outputable a)

-- ---------------------------------------------------------------------------
-- MuOp type

-- | A mutation operation: a before/after pair of the same node type.
-- Guard mutations use 'A' (the parent 'Alt_') because 'GRHS' lacks an
-- 'Outputable' instance, making the whole-match replacement the simplest
-- strategy that works with the 'MuNode' constraint.
data MuOp
    = E (Expr_,  Expr_)
    | D (Decl_,  Decl_)
    | A (Alt_,   Alt_)
    | S (Stmt_,  Stmt_)

-- | Dispatch a rank-2 function over the typed pair inside a 'MuOp'.
apply :: (forall a. MuNode a => (a, a) -> c) -> MuOp -> c
apply f (E m) = f m
apply f (D m) = f m
apply f (A m) = f m
apply f (S m) = f m

-- ---------------------------------------------------------------------------
-- Span extraction

-- | Extract the source span of the /before/ node as a @(startLine, startCol,
-- endLine, endCol)@ tuple, used by 'Test.Mutaskell.TestAdapter.Mutant'.
getSpan :: MuOp -> (Int, Int, Int, Int)
getSpan = apply go
  where
    go :: MuNode a => (a, a) -> (Int, Int, Int, Int)
    go (a, _) = case getHasLoc a of
        RealSrcSpan rss _ ->
            let s = realSrcSpanStart rss
                e = realSrcSpanEnd rss
            in (srcLocLine s, srcLocCol s, srcLocLine e, srcLocCol e)
        UnhelpfulSpan _ -> (0, 0, 0, 0)

-- ---------------------------------------------------------------------------
-- Identity check

{- | @'same' op@ is @True@ when the /before/ and /after/ nodes serialise to the
same string — i.e., the mutation is a no-op and should be discarded.
Uses 'ppr' so no @Eq@ instance on AST nodes is required.
-}
same :: MuOp -> Bool
same = apply $ \(a, b) ->
    showSDocUnsafe (ppr a) == showSDocUnsafe (ppr b)

-- ---------------------------------------------------------------------------
-- Core combinators

{- | Replace a specific node occurrence.

Matching is by source location (@'getHasLoc'@) rather than by value equality,
because GHC\'s AST types do not derive @Eq@.  Once a match is found,
'transferEntry' copies the original node\'s leading-whitespace delta onto the
replacement so that 'exactPrint' preserves layout.
-}
(~~>) :: (MonadPlus m, MuNode a) => a -> a -> a -> m a
x ~~> y = \z ->
    if getHasLoc x == getHasLoc z
    then return (transferEntry z y)
    else mzero

-- | Lift a 'MuOp' into a generic one-site transformation.
mkMpMuOp :: (MonadPlus m, G.Typeable a) => MuOp -> a -> m a
mkMpMuOp = apply $ G.mkMp . uncurry (~~>)

-- | Show a single mutation as @{ before } ==> { after }@.
showM :: MuNode a => (a, a) -> String
showM (s, t) =
    "{\n" ++ showSDocUnsafe (ppr s) ++ "\n} ==> {\n" ++ showSDocUnsafe (ppr t) ++ "\n}"

instance Show MuOp where
    show = apply showM

-- ---------------------------------------------------------------------------
-- Mutable class and pair-building operators

{- | A node type whose values can be paired into a 'MuOp'.
Each instance corresponds to one 'MuOp' constructor.
-}
class Mutable a where
    (==>) :: a -> a -> MuOp

-- | Pair one element with every element in the list.
(==>*) :: Mutable a => a -> [a] -> [MuOp]
x ==>* lst = map (x ==>) lst

-- | Pair every element of the first list with every element of the second.
(*==>*) :: Mutable a => [a] -> [a] -> [MuOp]
xs *==>* ys = concatMap (==>* ys) xs

-- Instances use the fully-expanded concrete types (not the type-family
-- aliases) because GHC prohibits type-family applications in instance heads
-- even with FlexibleInstances.

-- | 'Expr_' = 'LocatedA' ('HsExpr' 'GhcPs')
instance Mutable (LocatedA (HsExpr GhcPs)) where
    (==>) = (E .) . (,)

-- | 'Decl_' = 'LocatedA' ('HsDecl' 'GhcPs')
instance Mutable (LocatedA (HsDecl GhcPs)) where
    (==>) = (D .) . (,)

-- For the nested instances we must also expand LHsExpr GhcPs → LocatedA (HsExpr GhcPs)
-- because LHsExpr is itself a type-family alias (XRec GhcPs).

-- | 'Alt_' = 'LocatedA' ('Match' 'GhcPs' ('LocatedA' ('HsExpr' 'GhcPs')))
instance Mutable (LocatedA (Match GhcPs (LocatedA (HsExpr GhcPs)))) where
    (==>) = (A .) . (,)

-- | 'Stmt_' = 'LocatedA' ('StmtLR' 'GhcPs' 'GhcPs' ('LocatedA' ('HsExpr' 'GhcPs')))
instance Mutable (LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))) where
    (==>) = (S .) . (,)

-- Note: GuardedRhs_ / LGRHS GhcPs has no Outputable instance so it cannot
-- be used as a MuOp constructor.  Guard mutations are handled via Alt_ (A).
