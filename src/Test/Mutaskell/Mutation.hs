{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | This module handles the mutation of different patterns.
module Test.Mutaskell.Mutation where

import Data.Generics (Typeable, listify, mkMp)
import Data.List (isPrefixOf, nub, nubBy, partition, permutations)
-- In GHC 9.12, LHsBindsLR GhcPs GhcPs = [LHsBind GhcPs] (plain list, not Bag)

import GHC.Hs
import GHC.Parser.Annotation ()
import Language.Haskell.Syntax.Basic (Boxity (..))
import GHC.Types.SrcLoc
    ( GenLocated (..)
    , generatedSrcSpan
    , unLoc
    )
import GHC.Types.Name.Reader
    ( RdrName (..), rdrNameOcc, mkRdrUnqual )
import GHC.Types.Name.Occurrence
    ( occNameString, mkVarOcc, mkDataOcc )
import GHC.Types.SourceText
    ( SourceText (..), IntegralLit (..), FractionalLit (..)
    , FractionalExponentBase (..)
    )
import GHC.Data.FastString (unpackFS, mkFastString)
import Language.Haskell.Syntax.Module.Name ()
import Language.Haskell.Syntax.Extension ()
import GHC.Utils.Outputable (showSDocUnsafe, ppr)
import System.Process (readProcess)

import Language.Haskell.GHC.ExactPrint (exactPrint)
import Language.Haskell.GHC.ExactPrint.Parsers (parseModuleFromString)
import Language.Haskell.GHC.ExactPrint.Transform (setEntryDP, transferEntryDP)

import Test.Mutaskell.Config
import Test.Mutaskell.MuOp
import Test.Mutaskell.TestAdapter
import Test.Mutaskell.Tix
import Test.Mutaskell.Utils.Common
import Test.Mutaskell.Utils.Syb

-- ---------------------------------------------------------------------------
-- Helpers for constructing synthetic GHC AST nodes

-- | The GHC library directory, obtained at runtime.
getLibdir :: IO FilePath
getLibdir = fmap (filter (/= '\n')) $ readProcess "ghc" ["--print-libdir"] ""

-- | String name of an 'RdrName'.
rdrStr :: RdrName -> String
rdrStr = occNameString . rdrNameOcc

-- | Create a located expression node with empty annotation.
-- The entry delta will be repaired by '(~~>)' via 'transferEntry'.
mkL :: NoAnn ann => a -> GenLocated (EpAnn ann) a
mkL = L noAnn

-- | Create a located expression wrapping an 'HsExpr'.
mkExpr :: HsExpr GhcPs -> LHsExpr GhcPs
mkExpr = mkL

-- | Create a variable reference expression.
-- XVar GhcPs = NoExtField, so we use 'noExtField' rather than 'noAnn'.
mkVar :: String -> LHsExpr GhcPs
mkVar s = mkL (HsVar noExtField (L noAnn (mkRdrUnqual (mkVarOcc s))))

-- | Create a data constructor reference expression.
mkDataVar :: String -> LHsExpr GhcPs
mkDataVar s = mkL (HsVar noExtField (L noAnn (mkRdrUnqual (mkDataOcc s))))

-- | Create a function application expression.
-- XApp GhcPs = NoExtField.  The argument is given a one-space entry delta so
-- that 'exactPrint' renders "f x" rather than "fx".
mkApp :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
mkApp f e = mkL (HsApp noExtField f (setEntryDP e (SameLine 1)))

-- | Create an operator application expression.
-- XOpApp GhcPs = NoExtField.
mkOpApp :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
mkOpApp l op r = mkL (OpApp noExtField l op r)

-- | Create an overloaded integer literal expression.
-- XOverLitE GhcPs = NoExtField; XOverLit GhcPs = NoExtField.
mkIntLitExpr :: Integer -> LHsExpr GhcPs
mkIntLitExpr n = mkL (HsOverLit noExtField overlit)
  where
    overlit = OverLit
        { ol_ext = noExtField
        , ol_val = HsIntegral IL
            { il_text  = SourceText (mkFastString (show n))
            , il_neg   = False
            , il_value = n
            }
        }

-- | Create an overloaded fractional literal expression.
mkFracLitExpr :: Rational -> LHsExpr GhcPs
mkFracLitExpr f = mkL (HsOverLit noExtField overlit)
  where
    overlit = OverLit
        { ol_ext = noExtField
        , ol_val = HsFractional FL
            { fl_text     = SourceText (mkFastString (show f))
            , fl_neg      = False
            , fl_signi    = f
            , fl_exp      = 0
            , fl_exp_base = Base10
            }
        }

-- | Create a string literal expression.
-- XLitE GhcPs = NoExtField; HsString takes NoSourceText for synthetic nodes.
mkStringExpr :: String -> LHsExpr GhcPs
mkStringExpr s = mkL (HsLit noExtField (HsString NoSourceText (mkFastString s)))

-- | Wrap an expression in explicit parentheses: @e@ → @(e)@.
-- Needed when injecting a prefix application such as @not@ around an
-- operator expression, so that @not (n > 0)@ is produced rather than the
-- mis-parenthesised @not n > 0@ (which parses as @(not n) > 0@).
-- XPar GhcPs = (EpToken "(", EpToken ")"); we build both tokens with a
-- zero-width delta so the parens hug the wrapped expression.
mkPar :: LHsExpr GhcPs -> LHsExpr GhcPs
mkPar e = mkL (HsPar (lpar, rpar) (setEntryDP e (SameLine 0)))
  where
    lpar = EpTok (EpaDelta generatedSrcSpan (SameLine 0) [])
    rpar = EpTok (EpaDelta generatedSrcSpan (SameLine 0) [])

-- | Create an empty list expression @[]@.
-- We provide explicit bracket tokens so that 'exactPrint' emits @[]@ rather
-- than the empty string that results from @noAnn :: AnnList ()@.
mkListExpr :: LHsExpr GhcPs
mkListExpr = mkL (ExplicitList ann [])
  where
    epTok d = EpTok (EpaDelta generatedSrcSpan d [])
    ann = (noAnn :: AnnList ())
            { al_brackets = ListSquare (epTok (SameLine 0)) (epTok (SameLine 0)) }

-- ---------------------------------------------------------------------------
-- Public API

{- | Generate mutants using the default configuration.
-}
genMutants ::
    -- | The module file to mutate
    FilePath ->
    -- | Coverage information (@.tix@ file or empty)
    FilePath ->
    IO (Either String (Int, [Mutant]))
genMutants = genMutantsWith defaultConfig

{- | Generate mutants with a custom configuration.
-}
genMutantsWith ::
    Config ->
    FilePath ->
    FilePath ->
    IO (Either String (Int, [Mutant]))
genMutantsWith config filename tix = do
    src <- readFile filename
    eparsed <- getASTFromStr src
    case eparsed of
        Left err -> return (Left err)
        Right origAst -> do
            let modul   = getModuleName origAst
                mutants = genMutantsFromAST config origAst
            ec <- getUnCoveredPatches tix modul
            case ec of
                Left err -> return (Left err)
                Right c  -> return $ Right $ case c of
                    Nothing -> (-1, mutants)
                    Just v  -> (length mutants, removeUncovered v mutants)

-- | Remove mutants not covered by any test.
removeUncovered :: [Span] -> [Mutant] -> [Mutant]
removeUncovered uspans = filter mutantIsCovered
  where
    mutantIsCovered Mutant{..} = not $ any (insideSpan _mspan) uspans

-- | Get the module name from a parsed AST.
getModuleName :: Module_ -> String
getModuleName m =
    maybe "" (moduleNameString . unLoc) (hsmodName m)

{- | Generate mutants from a source string (used by tests and the CLI).
-}
genMutantsForSrc :: Config -> String -> IO (Either String [Mutant])
genMutantsForSrc config src =
    fmap (fmap (genMutantsFromAST config)) (getASTFromStr src)

{- | Generate mutants from a pre-parsed AST.
-}
genMutantsFromAST :: Config -> Module_ -> [Mutant]
genMutantsFromAST config = genMutantsWithExtra config []

{- | Like 'genMutantsFromAST' but also accepts additional custom selectors.
Third-party packages can inject custom mutation operators here; use
'MutateOther' with a descriptive name as the 'MuVar'.
-}
genMutantsWithExtra ::
    Config ->
    [Module_ -> [(MuVar, MuOp)]] ->
    Module_ ->
    [Mutant]
genMutantsWithExtra config extraSels origAst =
    nubBy (\a b -> _mutant a == _mutant b) $
        filter (\m -> _mutant m /= origStr) $
            map (toMutant . apTh exactPrint) $
                nubBy (\(v1,s1,_) (v2,s2,_) -> v1==v2 && s1==s2)
                    (mutatesN ops origAst 1)
  where
    -- Generate ops only from non-test declarations (to avoid mutating the test
    -- harness), but apply them to the full module so exactPrint can use every
    -- declaration's original EpAnn delta positions.
    (_, noAnnDecls) = splitAnnotations origAst
    opsAst  = putDecl origAst noAnnDecls
    ops     = applicableOps config opsAst ++ concatMap ($ opsAst) extraSels
    origStr = exactPrint origAst

-- | Produce all mutants using the default operator list.
programMutants :: Config -> Module_ -> [(MuVar, Span, Module_)]
programMutants config = programMutantsWith config []

-- | Like 'programMutants' but accepts additional custom selectors.
programMutantsWith ::
    Config ->
    [Module_ -> [(MuVar, MuOp)]] ->
    Module_ ->
    [(MuVar, Span, Module_)]
programMutantsWith config extraSels ast =
    nubBy (\(v1,s1,_) (v2,s2,_) -> v1==v2 && s1==s2) $
        mutatesN (applicableOps config ast ++ concatMap ($ ast) extraSels) ast 1

-- | All applicable mutation operators for the given module.
applicableOps :: Config -> Module_ -> [(MuVar, MuOp)]
applicableOps config ast = relevantOps ast opsList
  where
    opsList =
        concatMap spread
            [ (MutatePatternMatch,      selectFnMatches ast)
            , (MutateValues,            selectLiteralOps ast)
            , (MutateFunctions,         selectFunctionOps (muOp config) ast)
            , (MutateNegateIfElse,      selectIfElseBoolNegOps ast)
            , (MutateNegateGuards,      selectGuardedBoolNegOps ast)
            , (MutateOther "remove-not",          selectRemoveNotOps ast)
            , (MutateOther "remove-negation",     selectRemoveNegationOps ast)
            , (MutateOther "case-alt-remove",     selectCaseAltRemoveOps ast)
            , (MutateOther "case-default-remove", selectCaseDefaultRemoveOps ast)
            , (MutateOther "remove-stmt",         selectRemoveStmtOps ast)
            , (MutateOther "remove-let-binding",  selectRemoveLetBindingOps ast)
            , (MutateOther "remove-where-binding",selectRemoveWhereBindingOps ast)
            , (MutateOther "remove-self-assign",  selectRemoveSelfAssignOps ast)
            , (MutateOther "negate-literal",      selectNegateLiteralOps ast)
            , (MutateOther "string-literal",      selectStringLiteralOps ast)
            , (MutateOther "bool-operand",        selectBoolOperandOps ast)
            , (MutateOther "flip-maybe",          selectFlipMaybeOps ast)
            , (MutateOther "flip-either",         selectFlipEitherOps ast)
            , (MutateOther "remove-forkIO",       selectRemoveForkIOOps ast)
            , (MutateOther "bracket-degenerate",  selectBracketDegenerateOps ast)
            , (MutateOther "error-guard",         selectErrorGuardOps ast)
            , (MutateOther "replace-mutable-arg", selectReplaceMutableArgOps ast)
            , (MutateOther "zero-return",          selectZeroReturnOps ast)
            , (MutateOther "list-literal",         selectExplicitListOps ast)
            , (MutateOther "bind-to-sequence",     selectBindToSequenceOps ast)
            , (MutateOther "pattern-constructor",  selectPatternConstructorFlipOps ast)
            , (MutateOther "append-strip",         selectAppendStripOps ast)
            , (MutateOther "flip-args",            selectFlipArgsOps ast)
            , (MutateOther "seq-strip",            selectSeqStripOps ast)
            , (MutateOther "tuple-swap",           selectTupleSwapOps ast)
            , (MutateOther "ordering-literal",     selectOrderingLitOps ast)
            ]

-- ---------------------------------------------------------------------------
-- Module-level structural helpers

-- | Split declarations into test-annotated and non-annotated groups.
splitAnnotations :: Module_ -> ([Decl_], [Decl_])
splitAnnotations ast = partition fn (getDecl ast)
  where
    fn x = (functionName x ++ pragmaName x) `elem` getAnnotatedTests ast

-- | Get all annotated test names from the module.
-- Falls back to naming-convention auto-discovery when no ANN annotations exist.
getAnnotatedTests :: Module_ -> [String]
getAnnotatedTests ast =
    let byAnn = concatMap (getAnn ast) ["Test", "TestSupport"]
    in if null byAnn then autoDiscoverTestNames ast else byAnn

-- | Extract the declaration list from a parsed module.
getDecl :: Module_ -> [Decl_]
getDecl m = hsmodDecls m

-- | Replace the declaration list in a parsed module.
putDecl :: Module_ -> [Decl_] -> Module_
putDecl m decls = m { hsmodDecls = decls }

-- ---------------------------------------------------------------------------
-- Parsing and serialisation

-- | Parse a Haskell source string into a 'Module_'.
-- 'parseModuleFromString' returns @Located (HsModule GhcPs)@ (= 'ParsedSource');
-- we strip the outer 'Located' wrapper since mutations operate on the bare
-- 'HsModule GhcPs' and 'exactPrint' works on the bare type too.
getASTFromStr :: String -> IO (Either String Module_)
getASTFromStr src = do
    libdir <- getLibdir
    result <- parseModuleFromString libdir "<mucheck>" src
    return $ case result of
        Left msgs      -> Left (showSDocUnsafe (ppr msgs))
        Right (L _ m)  -> Right m

-- | Get all test function names from a source file (by path).
getAllTests :: String -> IO (Either String [String])
getAllTests modname = readFile modname >>= allTests

-- | Get all test function names from a source string.
allTests :: String -> IO (Either String [String])
allTests modsrc = do
    parsed <- getASTFromStr modsrc
    return $ case parsed of
        Left err  -> Left err
        Right ast -> Right (nub (getAnn ast "Test" ++ autoDiscoverTestNames ast))

-- | Get all @{-# ANN funcName "label" #-}@ annotations matching @label@.
getAnn :: Module_ -> String -> [String]
getAnn m label =
    [ rdrStr rdr
    | L _ (AnnD _ (HsAnnotation _ prov (L _ expr))) <- listify isAnnDecl m
    , matchesLabel label expr
    , rdr <- provRdr prov
    ]
  where
    isAnnDecl :: LHsDecl GhcPs -> Bool
    isAnnDecl (L _ AnnD{}) = True
    isAnnDecl _            = False

    matchesLabel :: String -> HsExpr GhcPs -> Bool
    matchesLabel expected (HsLit _ (HsString _ fs)) = unpackFS fs == expected
    matchesLabel _ _                                = False

    provRdr :: AnnProvenance GhcPs -> [RdrName]
    provRdr (ValueAnnProvenance (L _ rdr)) = [rdr]
    provRdr (TypeAnnProvenance  (L _ rdr)) = [rdr]
    provRdr ModuleAnnProvenance            = []

-- | Auto-discover test names by naming convention (@prop_*@, @test_*@, @spec_*@).
autoDiscoverTestNames :: Module_ -> [String]
autoDiscoverTestNames ast = filter isTestName $ map functionName (getDecl ast)
  where
    isTestName n = not (null n) && any (`isPrefixOf` n) ["prop_", "test_", "spec_"]

-- | Extract the string name of a function binding declaration.
functionName :: Decl_ -> String
functionName (L _ (ValD _ (FunBind _ fid _))) =
    rdrStr (unLoc fid)
functionName (L _ (ValD _ (PatBind _ (L _ (VarPat _ (L _ rdr))) _ _))) =
    rdrStr rdr
functionName _ = ""

-- | Extract the name of an @{-# ANN name ... #-}@ pragma declaration.
pragmaName :: Decl_ -> String
pragmaName (L _ (AnnD _ (HsAnnotation _ (ValueAnnProvenance (L _ rdr)) _))) =
    rdrStr rdr
pragmaName _ = ""

-- ---------------------------------------------------------------------------
-- Mutation application

{- | Apply mutation operators up to order @n@, returning mutated modules.
-}
mutatesN ::
    [(MuVar, MuOp)] ->
    Module_ ->
    Int ->
    [(MuVar, Span, Module_)]
mutatesN ops ast n = mutatesN' ops (MutateOther [], toSpan (0,0,0,0), ast) n
  where
    mutatesN' os ms 1 = concatMap (`mutate` ms) os
    mutatesN' os ms c =
        concat [mutatesN' os m 1 | m <- mutatesN' os ms (pred c)]

{- | Apply one mutation operator at exactly one site in the module.
Returns all single-site applications of the operator.
We no longer use @\\ [m]@ since 'once' with SrcSpan-based matching never
returns the original unchanged module.
-}
mutate :: (MuVar, MuOp) -> (MuVar, Span, Module_) -> [(MuVar, Span, Module_)]
mutate (v, op) (_, _, m) =
    map (v, toSpan (getSpan op),) $ once (mkMpMuOp op) m

-- | Sub-arrays with one fewer element (returns @[]@ for a singleton list).
-- 'choose' uses 'subsequences' internally and does not require 'Eq'.
removeOneElem :: [t] -> [[t]]
removeOneElem [_] = []
removeOneElem l   = choose l (length l - 1)

-- | Replace the element at index @i@ with @x@.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- ---------------------------------------------------------------------------
-- Generic selector helper

{- | 'selectValOps' finds all nodes of type @b@ matching @predicate@, applies
@f@ to each to get replacement candidates, and returns the resulting 'MuOp' list.
-}
selectValOps :: (Typeable b, Mutable b) => (b -> Bool) -> (b -> [b]) -> Module_ -> [MuOp]
selectValOps predicate f m =
    concat [x ==>* f x | x <- listify predicate m]

-- ---------------------------------------------------------------------------
-- Literal value mutations

-- | Mutations of literal values (integers, fractions, chars, strings, booleans).
selectLiteralOps :: Module_ -> [MuOp]
selectLiteralOps m = selectLitOps m ++ selectBLitOps m

-- | Mutations of monomorphic and overloaded numeric/char/string literals.
selectLitOps :: Module_ -> [MuOp]
selectLitOps m = selectValOps isLitExpr toLitVariants m
  where
    isLitExpr :: LHsExpr GhcPs -> Bool
    isLitExpr (L _ (HsLit _ _))     = True
    isLitExpr (L _ (HsOverLit _ _)) = True
    isLitExpr _                     = False

    toLitVariants :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    -- Monomorphic integer prims
    toLitVariants (L _ (HsLit _ (HsIntPrim _ n))) =
        map mkL [HsLit noExtField (HsIntPrim NoSourceText v) | v <- nub [n+1, n-1, 0, 1], v /= n]
    toLitVariants (L _ (HsLit _ (HsWordPrim _ n))) =
        map mkL [HsLit noExtField (HsWordPrim NoSourceText v) | v <- nub [n+1, n-1, 0, 1], v /= n]
    -- Monomorphic char
    toLitVariants (L _ (HsLit _ (HsChar _ c))) =
        map mkL [HsLit noExtField (HsChar NoSourceText v) | v <- [pred c, succ c]]
    toLitVariants (L _ (HsLit _ (HsCharPrim _ c))) =
        map mkL [HsLit noExtField (HsCharPrim NoSourceText v) | v <- [pred c, succ c]]
    -- Monomorphic string
    toLitVariants (L _ (HsLit _ (HsString _ _))) =
        [mkL (HsLit noExtField (HsString NoSourceText (mkFastString "")))]
    toLitVariants (L _ (HsLit _ (HsStringPrim _ _))) =
        [mkL (HsLit noExtField (HsString NoSourceText (mkFastString "")))]
    -- Overloaded integer (Num): reuse the original ol_ext field for annotation fidelity
    toLitVariants (L _ (HsOverLit _ ol@OverLit{ ol_val = HsIntegral il })) =
        let n = il_value il
            vals = nub [n+1, n-1, 0, 1]
        in [ mkL (HsOverLit noExtField ol{ ol_val = HsIntegral il{ il_value = v, il_text = SourceText (mkFastString (show v)) } })
           | v <- vals ]
    -- Overloaded fractional (Fractional)
    toLitVariants (L _ (HsOverLit _ ol@OverLit{ ol_val = HsFractional fl })) =
        let f = fl_signi fl
            vals = nub [f+1, f-1, 0, 1]
        in [ mkL (HsOverLit noExtField ol{ ol_val = HsFractional fl{ fl_signi = v } })
           | v <- vals ]
    toLitVariants _ = []

-- | Mutations of boolean literals (@True@ ↔ @False@).
selectBLitOps :: Module_ -> [MuOp]
selectBLitOps m = selectValOps isBoolVar convertBool m
  where
    isBoolVar :: LHsExpr GhcPs -> Bool
    isBoolVar (L _ (HsVar _ (L _ rdr))) = rdrStr rdr `elem` ["True", "False"]
    isBoolVar _                         = False

    convertBool :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convertBool (L _ (HsVar _ (L _ rdr)))
        | rdrStr rdr == "True"  = [mkDataVar "False"]
        | rdrStr rdr == "False" = [mkDataVar "True"]
    convertBool _ = []

-- ---------------------------------------------------------------------------
-- Control-flow mutations

-- | Swap then/else branches of @if@ expressions.
selectIfElseBoolNegOps :: Module_ -> [MuOp]
selectIfElseBoolNegOps m = selectValOps isIf convert m
  where
    isIf :: LHsExpr GhcPs -> Bool
    isIf (L _ HsIf{}) = True
    isIf _            = False

    -- Swap the then/else branches.  Each branch carries its own leading entry
    -- delta (the spacing after the @then@/@else@ keyword tokens held in @x@);
    -- swapping the branches verbatim leaves each branch with the *other*
    -- branch's delta, which corrupts layout and drops the @else@ keyword,
    -- yielding source that never compiles.  We transfer the original branch's
    -- entry delta onto the branch that now occupies its slot (the same
    -- technique 'fixEntries' uses for clause reordering).
    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsIf x cond t f)) =
        [mkL (HsIf x cond (transferEntryDP t f) (transferEntryDP f t))]
    convert _ = []

-- | Negate boolean guards: @x == 1@ → @not (x == 1)@.
-- We match at the 'Alt_' (whole match) level because 'GRHS' has no
-- 'Outputable' instance, making 'GuardedRhs_' unsuitable for 'MuOp'.
selectGuardedBoolNegOps :: Module_ -> [MuOp]
selectGuardedBoolNegOps m = selectValOps isMatchWithGuards convert m
  where
    isMatchWithGuards :: Alt_ -> Bool
    isMatchWithGuards (L _ (Match _ _ _ (GRHSs _ grhss _))) =
        any hasNonOtherwiseGuard grhss

    hasNonOtherwiseGuard :: GuardedRhs_ -> Bool
    hasNonOtherwiseGuard (L _ (GRHS _ stmts _)) =
        any (not . isOtherwiseStmt) stmts && not (null stmts)

    isOtherwiseStmt :: ExprLStmt GhcPs -> Bool
    isOtherwiseStmt (L _ (BodyStmt _ (L _ (HsVar _ (L _ rdr))) _ _)) =
        rdrStr rdr == "otherwise"
    isOtherwiseStmt _ = False

    convert :: Alt_ -> [Alt_]
    convert (L _ (Match xm ctx pats (GRHSs xg grhss binds))) =
        [ mkL (Match xm ctx pats (GRHSs xg (replaceAt i grhs' grhss) binds))
        | (i, grhs) <- zip [0..] grhss
        , grhs' <- convertGrhs grhs
        ]

    convertGrhs :: GuardedRhs_ -> [GuardedRhs_]
    convertGrhs (L lg (GRHS x stmts body)) =
        [ L lg (GRHS x stmts' body)
        | stmts' <- once (mkMp boolNegate) stmts
        ]

    boolNegate :: ExprLStmt GhcPs -> [ExprLStmt GhcPs]
    boolNegate s | isOtherwiseStmt s = []
    boolNegate (L _ (BodyStmt x expr y z)) =
        [mkL (BodyStmt x (mkApp (mkVar "not") (mkPar expr)) y z)]
    boolNegate _ = []

-- ---------------------------------------------------------------------------
-- Pattern-match mutations

-- | Permute / remove clauses in function definitions.
selectFnMatches :: Module_ -> [MuOp]
selectFnMatches m = selectValOps isFunDecl convert m
  where
    isFunDecl :: Decl_ -> Bool
    isFunDecl (L _ (ValD _ FunBind{})) = True
    isFunDecl _                        = False

    convert :: Decl_ -> [Decl_]
    convert (L _ (ValD xv (FunBind xb fid (MG xmg (L lms ms))))) =
        -- Re-assign each match's entry delta from the corresponding original
        -- position so that exactPrint places clauses on the correct lines
        -- whether we reorder or remove them.
        [ mkL (ValD xv (FunBind xb fid (MG xmg (L lms (fixEntries ms ms')))))
        | ms' <- permutations ms ++ removeOneElem ms
        ]
    convert _ = []

    -- Copy each original match's leading-whitespace delta to the match at the
    -- same position in the modified list.  This ensures correctness for both
    -- clause removal (ms' shorter than ms) and reordering.
    fixEntries :: [Alt_] -> [Alt_] -> [Alt_]
    fixEntries origMs newMs = zipWith transferEntryDP origMs newMs

-- ---------------------------------------------------------------------------
-- Function / operator substitution

-- | Substitute symbolic operator names.
selectSymbolFnOps :: Module_ -> [String] -> [MuOp]
selectSymbolFnOps m syms = selectValOps isSym convert m
  where
    isSym :: LHsExpr GhcPs -> Bool
    isSym (L _ (HsVar _ (L _ rdr))) = rdrStr rdr `elem` syms
    isSym _                         = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsVar _ (L _ rdr))) =
        [ mkVar s | s <- filter (/= rdrStr rdr) syms ]
    convert _ = []

-- | Substitute identifier (non-operator) function names.
selectIdentFnOps :: Module_ -> [String] -> [MuOp]
selectIdentFnOps m idents = selectValOps isIdent convert m
  where
    isIdent :: LHsExpr GhcPs -> Bool
    isIdent (L _ (HsVar _ (L _ rdr))) = rdrStr rdr `elem` idents
    isIdent _                         = False

    -- Preserve the original located wrappers (entry delta and, crucially, the
    -- name's backquote adornment for infix use such as @x `div` y@) and swap
    -- only the 'OccName'.  Building a fresh 'mkVar' instead would drop the
    -- backquotes, emitting @x quot y@ — a type error that is silently skipped.
    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L l (HsVar x (L lr rdr))) =
        [ L l (HsVar x (L lr (mkRdrUnqual (mkVarOcc s))))
        | s <- filter (/= rdrStr rdr) idents
        ]
    convert _ = []

-- | Combined function/operator substitution based on 'Config' 'FnOp' entries.
selectFunctionOps :: [FnOp] -> Module_ -> [MuOp]
selectFunctionOps fos m = concatMap (selectIdentFnOps m) idents
                       ++ concatMap (selectSymbolFnOps m) syms
  where
    idents = map _fns $ filter (\a -> _type a == FnIdent)  fos
    syms   = map _fns $ filter (\a -> _type a == FnSymbol) fos

-- ---------------------------------------------------------------------------
-- Logical operator mutations

-- | Remove @not@ application: @not e@ → @e@.
selectRemoveNotOps :: Module_ -> [MuOp]
selectRemoveNotOps m = selectValOps isNotApp removeNot m
  where
    isNotApp :: LHsExpr GhcPs -> Bool
    isNotApp (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _)) = rdrStr rdr == "not"
    isNotApp _ = False

    removeNot :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    removeNot (L _ (HsApp _ _ e)) = [e]
    removeNot _ = []

-- | Remove negation: @negate e@ or @-e@ → @e@.
selectRemoveNegationOps :: Module_ -> [MuOp]
selectRemoveNegationOps m = selectValOps isNeg removeNeg m
  where
    isNeg :: LHsExpr GhcPs -> Bool
    isNeg (L _ (NegApp _ _ _))                             = True
    isNeg (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _))
        | rdrStr rdr == "negate"                           = True
    isNeg _ = False

    removeNeg :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    removeNeg (L _ (NegApp _ e _))   = [e]
    removeNeg (L _ (HsApp _ _ e))    = [e]
    removeNeg _ = []

-- ---------------------------------------------------------------------------
-- Case expression mutations

-- | Remove one alternative from a @case@ expression.
selectCaseAltRemoveOps :: Module_ -> [MuOp]
selectCaseAltRemoveOps m = selectValOps isCase convert m
  where
    isCase :: LHsExpr GhcPs -> Bool
    isCase (L _ (HsCase _ _ (MG _ (L _ alts)))) = length alts > 1
    isCase _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsCase x scrut (MG xmg (L la alts)))) =
        [ mkL (HsCase x scrut (MG xmg (L la alts')))
        | alts' <- removeOneElem alts
        ]
    convert _ = []

-- | Remove the catch-all / @otherwise@ alternative from @case@ and guards.
selectCaseDefaultRemoveOps :: Module_ -> [MuOp]
selectCaseDefaultRemoveOps m = caseAltDefault m ++ guardDefault m
  where
    -- Case: remove the wildcard / @otherwise@ alt
    caseAltDefault :: Module_ -> [MuOp]
    caseAltDefault = selectValOps isCaseWithDefault convertCaseDefault

    isCaseWithDefault :: LHsExpr GhcPs -> Bool
    isCaseWithDefault (L _ (HsCase _ _ (MG _ (L _ alts)))) =
        any isDefaultAlt alts && length alts > 1
    isCaseWithDefault _ = False

    isDefaultAlt :: Alt_ -> Bool
    isDefaultAlt (L _ (Match _ _ (L _ [L _ (WildPat _)]) _)) = True
    isDefaultAlt (L _ (Match _ _ (L _ [L _ (VarPat _ (L _ rdr))]) _))
        = rdrStr rdr == "otherwise"
    isDefaultAlt _ = False

    convertCaseDefault :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convertCaseDefault (L _ (HsCase x scrut (MG xmg (L la alts)))) =
        [mkL (HsCase x scrut (MG xmg (L la (filter (not . isDefaultAlt) alts))))]
    convertCaseDefault _ = []

    -- Guards: remove the @otherwise@ guarded RHS from function matches.
    -- We match at the Alt_ (LMatch) level for the same reason as
    -- selectGuardedBoolNegOps: GRHS lacks an Outputable instance.
    guardDefault :: Module_ -> [MuOp]
    guardDefault = selectValOps isMatchWithDefaultGuard convertMatchDefault2

    isMatchWithDefaultGuard :: Alt_ -> Bool
    isMatchWithDefaultGuard (L _ (Match _ _ _ (GRHSs _ grhss _))) =
        any isDefaultGRHS grhss && length grhss > 1

    isDefaultGRHS :: GuardedRhs_ -> Bool
    isDefaultGRHS (L _ (GRHS _ stmts _)) = any isOtherwiseStmt stmts

    isOtherwiseStmt :: ExprLStmt GhcPs -> Bool
    isOtherwiseStmt (L _ (BodyStmt _ (L _ (HsVar _ (L _ rdr))) _ _)) =
        rdrStr rdr == "otherwise"
    isOtherwiseStmt _ = False

    convertMatchDefault2 :: Alt_ -> [Alt_]
    convertMatchDefault2 (L _ (Match xm ctx pats (GRHSs xg grhss binds))) =
        [mkL (Match xm ctx pats (GRHSs xg (filter (not . isDefaultGRHS) grhss) binds))]

-- ---------------------------------------------------------------------------
-- Do-block mutations

-- | Remove one statement from a @do@ block (skips result-binding statements).
selectRemoveStmtOps :: Module_ -> [MuOp]
selectRemoveStmtOps m = selectValOps isDo convert m
  where
    -- List comprehensions (ListComp / MonadComp) have a mandatory LastStmt as
    -- their final statement; removing any statement with the isValidDo check
    -- can strip that LastStmt, producing a bodyless comprehension that makes
    -- GHC's pprComp panic.  Only match proper do/mdo blocks.
    isDo :: LHsExpr GhcPs -> Bool
    isDo (L _ (HsDo _ (DoExpr _)  _)) = True
    isDo (L _ (HsDo _ (MDoExpr _) _)) = True
    isDo _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsDo x ctx (L ls stmts))) =
        [ mkL (HsDo x ctx (L ls stmts'))
        | stmts' <- removeOneStmt stmts
        ]
    convert _ = []

    removeOneStmt :: [ExprLStmt GhcPs] -> [[ExprLStmt GhcPs]]
    removeOneStmt stmts =
        [ take i stmts ++ drop (i+1) stmts
        | i <- [0 .. length stmts - 1]
        , isValidDo (take i stmts ++ drop (i+1) stmts)
        ]

    isValidDo :: [ExprLStmt GhcPs] -> Bool
    isValidDo [] = False
    isValidDo ss = case last ss of
        L _ (BodyStmt _ _ _ _)  -> True
        _                       -> False

-- ---------------------------------------------------------------------------
-- Let/where binding mutations

-- | Remove one binding from @let...in@ expressions and @do@-block @let@s.
selectRemoveLetBindingOps :: Module_ -> [MuOp]
selectRemoveLetBindingOps m =
    selectValOps isLetExpr convertLet m ++
    selectValOps isDoWithLet convertDo m
  where
    isLetExpr :: LHsExpr GhcPs -> Bool
    isLetExpr (L _ (HsLet _ (HsValBinds _ (ValBinds _ bag _)) _)) =
        not (null bag)
    isLetExpr _ = False

    convertLet :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convertLet (L _ (HsLet x (HsValBinds xv (ValBinds xvb bag sigs)) body)) =
        let bs = bag
        in [ mkL (HsLet x (HsValBinds xv (ValBinds xvb (bs') sigs)) body)
           | bs' <- removeOneElem bs
           ]
    convertLet _ = []

    isDoWithLet :: LHsExpr GhcPs -> Bool
    isDoWithLet (L _ (HsDo _ _ (L _ stmts))) = any isLetStmt stmts
    isDoWithLet _ = False

    isLetStmt :: ExprLStmt GhcPs -> Bool
    isLetStmt (L _ (LetStmt _ (HsValBinds _ (ValBinds _ bag _)))) =
        not (null bag)
    isLetStmt _ = False

    convertDo :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convertDo (L _ (HsDo x ctx (L ls stmts))) =
        [ mkL (HsDo x ctx (L ls (replaceAt i s' stmts)))
        | (i, s) <- zip [0..] stmts
        , s' <- convertLetStmt s
        ]
    convertDo _ = []

    convertLetStmt :: ExprLStmt GhcPs -> [ExprLStmt GhcPs]
    convertLetStmt (L _ (LetStmt x (HsValBinds xv (ValBinds xvb bag sigs)))) =
        let bs = bag
        in [ mkL (LetStmt x (HsValBinds xv (ValBinds xvb (bs') sigs)))
           | bs' <- removeOneElem bs
           ]
    convertLetStmt _ = []

-- | Remove one binding from @where@ clauses.
selectRemoveWhereBindingOps :: Module_ -> [MuOp]
selectRemoveWhereBindingOps m =
    selectValOps isFunWithWhere convertFun m ++
    selectValOps isPatWithWhere  convertPat  m
  where
    isFunWithWhere :: Decl_ -> Bool
    isFunWithWhere (L _ (ValD _ (FunBind _ _ (MG _ (L _ ms))))) =
        any matchHasWhere ms
    isFunWithWhere _ = False

    matchHasWhere :: Alt_ -> Bool
    matchHasWhere (L _ (Match _ _ _ (GRHSs _ _ (HsValBinds _ (ValBinds _ bag _))))) =
        not (null bag)
    matchHasWhere _ = False

    convertFun :: Decl_ -> [Decl_]
    convertFun (L _ (ValD xv (FunBind xb fid (MG xmg (L lms ms))))) =
        [ mkL (ValD xv (FunBind xb fid (MG xmg (L lms (replaceAt i m' ms)))))
        | (i, match_) <- zip [0..] ms
        , m' <- convertMatch match_
        ]
    convertFun _ = []

    convertMatch :: Alt_ -> [Alt_]
    -- Preserve the outer 'L la' annotation so exactPrint knows where to place
    -- the match after the where-binding is removed.
    convertMatch (L la (Match xm ctx pats (GRHSs xg grhss (HsValBinds xv (ValBinds xvb bag sigs))))) =
        [ L la (Match xm ctx pats (GRHSs xg grhss (HsValBinds xv (ValBinds xvb bs' sigs))))
        | bs' <- removeOneElem bag
        ]
    convertMatch _ = []

    isPatWithWhere :: Decl_ -> Bool
    isPatWithWhere (L _ (ValD _ (PatBind _ _ _ (GRHSs _ _ (HsValBinds _ (ValBinds _ bag _)))))) =
        not (null bag)
    isPatWithWhere _ = False

    convertPat :: Decl_ -> [Decl_]
    convertPat (L _ (ValD xv (PatBind xb pat mult (GRHSs xg grhss (HsValBinds xhv (ValBinds xvb bs sigs)))))) =
        [ mkL (ValD xv (PatBind xb pat mult (GRHSs xg grhss (HsValBinds xhv (ValBinds xvb bs' sigs)))))
        | bs' <- removeOneElem bs
        ]
    convertPat _ = []

-- ---------------------------------------------------------------------------
-- Self-assignment removal

-- | Remove @let x = x@ and @x <- return x@ self-assignments.
selectRemoveSelfAssignOps :: Module_ -> [MuOp]
selectRemoveSelfAssignOps m =
    selectValOps isLetWithSelf convertLet m ++
    selectValOps isDoWithSelf  convertDo  m
  where
    isLetWithSelf :: LHsExpr GhcPs -> Bool
    isLetWithSelf (L _ (HsLet _ (HsValBinds _ (ValBinds _ bag _)) _)) =
        any isSelfAssignBind (bag)
    isLetWithSelf _ = False

    isSelfAssignBind :: LHsBind GhcPs -> Bool
    -- GHC parses `x = x` in let-bindings as FunBind (function with no patterns).
    isSelfAssignBind (L _ (FunBind _ (L _ rdr1)
                           (MG _ (L _ [L _ (Match _ _ (L _ [])
                               (GRHSs _ [L _ (GRHS _ [] (L _ (HsVar _ (L _ rdr2))))] _))])))) =
        rdrStr rdr1 == rdrStr rdr2
    -- Fallback: PatBind-style variable binding `x = x`.
    isSelfAssignBind (L _ (PatBind _ (L _ (VarPat _ (L _ rdr1)))
                           _mult
                           (GRHSs _ [L _ (GRHS _ [] (L _ (HsVar _ (L _ rdr2))))] _))) =
        rdrStr rdr1 == rdrStr rdr2
    isSelfAssignBind _ = False

    convertLet :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convertLet (L _ (HsLet x (HsValBinds xv (ValBinds xvb bag sigs)) body)) =
        let bs = bag
            bs' = filter (not . isSelfAssignBind) bs
        in [mkL (HsLet x (HsValBinds xv (ValBinds xvb (bs') sigs)) body)]
    convertLet _ = []

    isDoWithSelf :: LHsExpr GhcPs -> Bool
    isDoWithSelf (L _ (HsDo _ _ (L _ stmts))) = any isSelfAssignStmt stmts
    isDoWithSelf _ = False

    isSelfAssignStmt :: ExprLStmt GhcPs -> Bool
    isSelfAssignStmt (L _ (BindStmt _ (L _ (VarPat _ (L _ rdr1)))
                            (L _ (HsApp _ (L _ (HsVar _ (L _ ret)))
                                          (L _ (HsVar _ (L _ rdr2))))))) =
        rdrStr rdr1 == rdrStr rdr2 && rdrStr ret == "return"
    isSelfAssignStmt _ = False

    convertDo :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convertDo (L _ (HsDo x ctx (L ls stmts))) =
        [mkL (HsDo x ctx (L ls (filter (not . isSelfAssignStmt) stmts)))]
    convertDo _ = []

-- ---------------------------------------------------------------------------
-- Numeric literal negation

-- | Replace positive integer/fraction literals with @negate x@.
selectNegateLiteralOps :: Module_ -> [MuOp]
selectNegateLiteralOps m = selectValOps isPosLit convert m
  where
    isPosLit :: LHsExpr GhcPs -> Bool
    isPosLit (L _ (HsOverLit _ OverLit{ ol_val = HsIntegral    IL{ il_value = n } })) = n > 0
    isPosLit (L _ (HsOverLit _ OverLit{ ol_val = HsFractional  FL{ fl_signi = f } })) = f > 0
    isPosLit _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    -- Build a fresh literal so the argument does not carry its original source
    -- positions, which would corrupt exactPrint when wrapped in `negate`.
    convert (L _ (HsOverLit _ OverLit{ol_val = HsIntegral il})) =
        [mkApp (mkVar "negate") (mkIntLitExpr (il_value il))]
    convert (L _ (HsOverLit _ OverLit{ol_val = HsFractional fl})) =
        [mkApp (mkVar "negate") (mkFracLitExpr (fl_signi fl))]
    convert _ = []

-- ---------------------------------------------------------------------------
-- String literal mutation

-- | Replace non-empty string literals in @==@ / @/=@ comparisons with @""@.
selectStringLiteralOps :: Module_ -> [MuOp]
selectStringLiteralOps m = selectValOps isStringComp convert m
  where
    isStringComp :: LHsExpr GhcPs -> Bool
    isStringComp (L _ (OpApp _ e1 (L _ (HsVar _ (L _ op))) e2)) =
        rdrStr op `elem` ["==", "/="] &&
        (isNonEmptyStr e1 || isNonEmptyStr e2)
    isStringComp _ = False

    isNonEmptyStr :: LHsExpr GhcPs -> Bool
    isNonEmptyStr (L _ (HsLit _ (HsString _ fs))) = not (null (unpackFS fs))
    isNonEmptyStr _ = False

    emptyStr :: LHsExpr GhcPs
    emptyStr = mkStringExpr ""

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (OpApp x e1 op e2)) =
        [mkL (OpApp x (if isNonEmptyStr e1 then emptyStr else e1)
                      op
                      (if isNonEmptyStr e2 then emptyStr else e2))]
    convert _ = []

-- ---------------------------------------------------------------------------
-- Boolean operand mutation

-- | Replace operands of @&&@ / @||@ with @True@ or @False@.
selectBoolOperandOps :: Module_ -> [MuOp]
selectBoolOperandOps m = selectValOps isBoolOp convert m
  where
    isBoolOp :: LHsExpr GhcPs -> Bool
    isBoolOp (L _ (OpApp _ _ (L _ (HsVar _ (L _ op))) _)) =
        rdrStr op `elem` ["&&", "||"]
    isBoolOp _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (OpApp x e1 op e2)) =
        [ mkL (OpApp x (mkDataVar "True")  op e2)
        , mkL (OpApp x (mkDataVar "False") op e2)
        , mkL (OpApp x e1 op (mkDataVar "True"))
        , mkL (OpApp x e1 op (mkDataVar "False"))
        ]
    convert _ = []

-- ---------------------------------------------------------------------------
-- Maybe / Either mutations

-- | Flip @Just x@ ↔ @Nothing@ and vice versa.
selectFlipMaybeOps :: Module_ -> [MuOp]
selectFlipMaybeOps m = selectValOps isMaybe convert m
  where
    isMaybe :: LHsExpr GhcPs -> Bool
    isMaybe (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _)) = rdrStr rdr == "Just"
    isMaybe (L _ (HsVar _ (L _ rdr)))                    = rdrStr rdr == "Nothing"
    isMaybe _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsApp _ (L _ (HsVar _ _)) _)) =
        [mkDataVar "Nothing"]
    convert (L _ (HsVar _ _)) =
        [mkApp (mkDataVar "Just") (mkVar "undefined")]
    convert _ = []

-- | Flip @Right x@ ↔ @Left x@ and @Left x@ ↔ @Right x@.
selectFlipEitherOps :: Module_ -> [MuOp]
selectFlipEitherOps m = selectValOps isEither convert m
  where
    isEither :: LHsExpr GhcPs -> Bool
    isEither (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _)) =
        rdrStr rdr `elem` ["Right", "Left"]
    isEither (L _ (HsVar _ (L _ rdr))) =
        rdrStr rdr `elem` ["Right", "Left"]
    isEither _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) e))
        | rdrStr rdr == "Right" = [mkApp (mkDataVar "Left")  e]
        | rdrStr rdr == "Left"  = [mkApp (mkDataVar "Right") e]
    convert (L _ (HsVar _ (L _ rdr)))
        | rdrStr rdr == "Right" = [mkDataVar "Left"]
        | rdrStr rdr == "Left"  = [mkDataVar "Right"]
    convert _ = []

-- ---------------------------------------------------------------------------
-- Concurrency / resource mutations

-- | Strip @forkIO@ / @async@ / @withAsync@ wrappers.
selectRemoveForkIOOps :: Module_ -> [MuOp]
selectRemoveForkIOOps m = selectValOps isFork convert m
  where
    isFork :: LHsExpr GhcPs -> Bool
    isFork (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _)) =
        rdrStr rdr `elem` ["forkIO", "async"]
    isFork (L _ (HsApp _ (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _)) _)) =
        rdrStr rdr == "withAsync"
    isFork _ = False

    gtgtgt :: LHsExpr GhcPs
    gtgtgt = mkVar ">>"

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) e))
        | rdrStr rdr `elem` ["forkIO", "async"] = [e]
    convert (L _ (HsApp _ (L _ (HsApp _ (L _ (HsVar _ _)) e1)) (L _ (HsLam _ _ (MG _ (L _ [L _ (Match _ _ _ (GRHSs _ [L _ (GRHS _ [] e2)] _))]))))))
        = [mkOpApp e1 gtgtgt e2]
    convert (L _ (HsApp _ (L _ (HsApp _ (L _ (HsVar _ _)) e1)) e2))
        = [mkOpApp e1 gtgtgt e2]
    convert _ = []

-- | Replace @bracket acq rel act@ with @acq >>= act@.
selectBracketDegenerateOps :: Module_ -> [MuOp]
selectBracketDegenerateOps m = selectValOps isBracket convert m
  where
    isBracket :: LHsExpr GhcPs -> Bool
    isBracket (L _ (HsApp _ (L _ (HsApp _ (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _)) _)) _)) =
        rdrStr rdr == "bracket"
    isBracket _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsApp _ (L _ (HsApp _ (L _ (HsApp _ (L _ (HsVar _ _)) e1)) _)) e3)) =
        [mkOpApp e1 (mkVar ">>=") e3]
    convert _ = []

-- | Replace exception handlers with a no-op returning @undefined@.
selectErrorGuardOps :: Module_ -> [MuOp]
selectErrorGuardOps m = selectValOps isErrorOp convert m
  where
    isErrorOp :: LHsExpr GhcPs -> Bool
    isErrorOp (L _ (HsApp _ (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _)) _)) =
        rdrStr rdr `elem` ["catch", "handle"]
    isErrorOp (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _)) =
        rdrStr rdr == "try"
    isErrorOp _ = False

    -- Simplest meaningful mutation: strip the error-handling wrapper entirely.
    -- catch e _handler → e   (removes the catch)
    -- handle _handler e → e  (removes the handle)
    -- try e → return (Right e)  (always succeeds)
    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsApp _ (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) e1)) _))
        | rdrStr rdr == "catch"  = [e1]
        | rdrStr rdr == "handle" = [e1]
    convert (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) e))
        | rdrStr rdr == "try" =
            [mkApp (mkVar "return") (mkApp (mkDataVar "Right") e)]
    convert _ = []

-- | Replace @IORef@ / @MVar@ / @TVar@ arguments with @undefined@.
selectReplaceMutableArgOps :: Module_ -> [MuOp]
selectReplaceMutableArgOps m = selectValOps isMutableVar convert m
  where
    mutableNames :: [String]
    mutableNames = ["ref", "mvar", "tvar", "ior", "stref"]

    isMutableVar :: LHsExpr GhcPs -> Bool
    isMutableVar (L _ (HsVar _ (L _ rdr))) = rdrStr rdr `elem` mutableNames
    isMutableVar _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert _ = [mkVar "undefined"]

-- ---------------------------------------------------------------------------
-- Zero-return mutation

-- | Replace each function match body with the zero value for the declared
-- return type.  Only applies when a type signature is present in the same module.
selectZeroReturnOps :: Module_ -> [MuOp]
selectZeroReturnOps m =
    -- XValD GhcPs = NoExtField; XFunBind = NoExtField.
    -- Eq (Match GhcPs) doesn't exist; identity mutations filtered by genMutantsWithExtra.
    [ fromDecl ==> mkL (ValD noExtField (FunBind noExtField fid mg'))
    | fromDecl@(L _ (ValD _ (FunBind _ fid (MG xmg (L lms ms))))) <- decls
    , let fname = occNameString (rdrNameOcc (unLoc fid))
    , Just retTy <- [lookup fname typeSigs]
    , Just zv    <- [typeZeroVal retTy]
    , let ms' = map (replaceMatchBody zv) ms
          mg' = MG xmg (L lms ms')
    ]
  where
    decls    = hsmodDecls m
    -- TypeSig _ ns ty  where ty :: LHsSigWcType GhcPs = HsWildCardBndrs GhcPs (LHsSigType GhcPs)
    typeSigs = [ (occNameString (rdrNameOcc (unLoc n)), returnType (unLoc (sig_body sig)))
               | L _ (SigD _ (TypeSig _ ns (HsWC _ (L _ sig)))) <- decls
               , n <- ns
               ]

    returnType :: HsType GhcPs -> HsType GhcPs
    returnType (HsFunTy _ _ _ ret) = returnType (unLoc ret)
    returnType (HsParTy _ ty)      = returnType (unLoc ty)
    returnType t                   = t

    typeZeroVal :: HsType GhcPs -> Maybe (LHsExpr GhcPs)
    typeZeroVal (HsTyVar _ _ (L _ rdr)) = case rdrStr rdr of
        "Bool"    -> Just (mkDataVar "False")
        "Int"     -> Just (mkIntLitExpr 0)
        "Integer" -> Just (mkIntLitExpr 0)
        "Double"  -> Just (mkFracLitExpr 0.0)
        "Float"   -> Just (mkFracLitExpr 0.0)
        "String"  -> Just (mkStringExpr "")
        _         -> Nothing
    typeZeroVal (HsListTy _ _)  = Just mkListExpr
    typeZeroVal (HsAppTy _ (L _ (HsTyVar _ _ (L _ rdr))) _) = case rdrStr rdr of
        "Maybe" -> Just (mkDataVar "Nothing")
        "IO"    -> Just (mkApp (mkVar "return") (mkVar "undefined"))
        _       -> Nothing
    typeZeroVal _ = Nothing

    replaceMatchBody :: LHsExpr GhcPs -> Alt_ -> Alt_
    -- Preserve the first GRHS's located annotation (which encodes the `=`
    -- position) so exactPrint can render "= zv" correctly.
    replaceMatchBody zv (L la (Match xm ctx pats (GRHSs xg (L lg (GRHS xga _ _):_) binds))) =
        L la (Match xm ctx pats
               (GRHSs xg [L lg (GRHS xga [] (setEntryDP zv (SameLine 1)))] binds))
    replaceMatchBody _ a = a

-- ---------------------------------------------------------------------------
-- Explicit list literal mutations

-- | Replace non-empty explicit list literals with the empty list or with
-- one element removed: @[x, y, z]@ → @[]@, @[x, z]@, @[y, z]@, @[x, y]@.
selectExplicitListOps :: Module_ -> [MuOp]
selectExplicitListOps m = selectValOps isNonEmptyList convert m
  where
    isNonEmptyList :: LHsExpr GhcPs -> Bool
    isNonEmptyList (L _ (ExplicitList _ elems)) = not (null elems)
    isNonEmptyList _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (ExplicitList ann elems)) =
        mkListExpr :
        [ mkL (ExplicitList ann elems') | elems' <- removeOneElem elems ]
    convert _ = []

-- ---------------------------------------------------------------------------
-- Monadic bind-to-sequence mutation

-- | Replace @x \<- action@ with @_ \<- action@ (wildcard bind) in @do@ blocks,
-- dropping the bound name so any downstream use of @x@ becomes a compile error
-- (killed mutant) or the mutation survives only if @x@ was already unused.
selectBindToSequenceOps :: Module_ -> [MuOp]
selectBindToSequenceOps m = selectValOps isDo convert m
  where
    isDo :: LHsExpr GhcPs -> Bool
    isDo (L _ (HsDo _ (DoExpr _)  _)) = True
    isDo (L _ (HsDo _ (MDoExpr _) _)) = True
    isDo _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsDo x ctx (L ls stmts))) =
        [ mkL (HsDo x ctx (L ls stmts'))
        | stmts' <- dropOneBind stmts
        ]
    convert _ = []

    dropOneBind :: [ExprLStmt GhcPs] -> [[ExprLStmt GhcPs]]
    dropOneBind stmts =
        [ replaceAt i (toWildBind s) stmts
        | (i, s) <- zip [0..] stmts
        , isNamedBind s
        , i < length stmts - 1
        ]

    isNamedBind :: ExprLStmt GhcPs -> Bool
    isNamedBind (L _ (BindStmt _ (L _ (VarPat _ _)) _)) = True
    isNamedBind _ = False

    toWildBind :: ExprLStmt GhcPs -> ExprLStmt GhcPs
    toWildBind (L l (BindStmt xb _ expr)) =
        L l (BindStmt xb (mkL (WildPat noExtField)) expr)
    toWildBind s = s

-- ---------------------------------------------------------------------------
-- Pattern constructor flip mutations

-- | Flip constructor patterns in function clauses and case alternatives:
-- @Just x@ ↔ @Nothing@, @Left x@ ↔ @Right x@, @True@ ↔ @False@ (top-level patterns only).
selectPatternConstructorFlipOps :: Module_ -> [MuOp]
selectPatternConstructorFlipOps m = selectValOps hasFlippableCon convert m
  where
    flippableSet :: [String]
    flippableSet = ["Just","Nothing","Left","Right","True","False"]

    flipConName :: String -> Maybe String
    flipConName "Just"    = Just "Nothing"
    flipConName "Nothing" = Just "Just"
    flipConName "Left"    = Just "Right"
    flipConName "Right"   = Just "Left"
    flipConName "True"    = Just "False"
    flipConName "False"   = Just "True"
    flipConName _         = Nothing

    isFlippablePat :: LPat GhcPs -> Bool
    isFlippablePat (L _ (ConPat _ (L _ rdr) _))     = rdrStr rdr `elem` flippableSet
    isFlippablePat (L _ (ParPat _ lp))               = isFlippablePat lp
    isFlippablePat _ = False

    hasFlippableCon :: Alt_ -> Bool
    hasFlippableCon (L _ (Match _ _ (L _ pats) _)) = any isFlippablePat pats

    convert :: Alt_ -> [Alt_]
    convert (L la (Match xm ctx (L lp pats) rhs)) =
        [ L la (Match xm ctx (L lp pats') rhs)
        | pats' <- flipOnePat pats
        ]

    flipOnePat :: [LPat GhcPs] -> [[LPat GhcPs]]
    flipOnePat pats =
        [ replaceAt i pat' pats
        | (i, pat) <- zip [0..] pats
        , pat' <- flipTopPat pat
        ]

    -- Unwrap ParPat and re-wrap the flipped result, so that function-argument
    -- patterns such as @f (Just x)@ and @f (Left e)@ are handled correctly.
    flipTopPat :: LPat GhcPs -> [LPat GhcPs]
    flipTopPat (L l (ParPat x inner)) =
        [ L l (ParPat x p) | p <- flipTopPat inner ]
    flipTopPat (L l (ConPat x (L lr rdr) args)) =
        case flipConName (rdrStr rdr) of
            Nothing        -> []
            Just "Nothing" ->
                [L l (ConPat x (L lr (mkRdrUnqual (mkDataOcc "Nothing"))) (PrefixCon [] []))]
            Just "Just"    ->
                [L l (ConPat x (L lr (mkRdrUnqual (mkDataOcc "Just")))
                         (PrefixCon [] [mkL (WildPat noExtField)]))]
            Just other     ->
                [L l (ConPat x (L lr (mkRdrUnqual (mkDataOcc other))) args)]
    flipTopPat _ = []

-- ---------------------------------------------------------------------------
-- Append strip mutation

-- | Replace @xs ++ ys@ with @xs@ or @ys@, testing that both halves are needed.
selectAppendStripOps :: Module_ -> [MuOp]
selectAppendStripOps m = selectValOps isAppend convert m
  where
    isAppend :: LHsExpr GhcPs -> Bool
    isAppend (L _ (OpApp _ _ (L _ (HsVar _ (L _ op))) _)) = rdrStr op == "++"
    isAppend _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (OpApp _ e1 _ e2)) = [e1, e2]
    convert _ = []

-- ---------------------------------------------------------------------------
-- Flip-argument mutation

-- | Swap the two arguments of a known binary function:
-- @f a b@ → @f b a@.  Applied only to functions where both arguments
-- typically share a type, reducing type-error noise.
selectFlipArgsOps :: Module_ -> [MuOp]
selectFlipArgsOps m = selectValOps isFlippable convert m
  where
    flippableFns :: [String]
    flippableFns =
        [ "compare", "max", "min", "gcd", "lcm"
        , "div", "mod", "quot", "rem"
        , "elem", "notElem"
        , "splitAt", "take", "drop", "replicate"
        ]

    isFlippable :: LHsExpr GhcPs -> Bool
    isFlippable (L _ (HsApp _ (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _)) _)) =
        rdrStr rdr `elem` flippableFns
    isFlippable _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsApp _ (L _ (HsApp _ f a)) b)) =
        [mkApp (mkApp f (setEntryDP b (SameLine 1))) (setEntryDP a (SameLine 1))]
    convert _ = []

-- ---------------------------------------------------------------------------
-- seq strip mutation

-- | Replace @seq x y@ with @y@, testing that forced evaluation is required.
selectSeqStripOps :: Module_ -> [MuOp]
selectSeqStripOps m = selectValOps isSeqApp convert m
  where
    isSeqApp :: LHsExpr GhcPs -> Bool
    isSeqApp (L _ (HsApp _ (L _ (HsApp _ (L _ (HsVar _ (L _ rdr))) _)) _)) =
        rdrStr rdr == "seq"
    isSeqApp _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsApp _ _ y)) = [y]
    convert _ = []

-- ---------------------------------------------------------------------------
-- Tuple swap mutation

-- | Swap the two components of a pair expression: @(a, b)@ → @(b, a)@.
-- Produces a compile error when @a@ and @b@ have different types; those
-- mutants are reported as killed via interpreter error.
selectTupleSwapOps :: Module_ -> [MuOp]
selectTupleSwapOps m = selectValOps isPair convert m
  where
    isPair :: LHsExpr GhcPs -> Bool
    isPair (L _ (ExplicitTuple _ [Present _ _, Present _ _] Boxed)) = True
    isPair _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (ExplicitTuple x [Present xa a, Present xb b] box)) =
        [mkL (ExplicitTuple x [Present xa b, Present xb a] box)]
    convert _ = []

-- ---------------------------------------------------------------------------
-- Ordering literal mutation

-- | Flip @GT@ ↔ @LT@ and replace @EQ@ with @GT@ or @LT@.
selectOrderingLitOps :: Module_ -> [MuOp]
selectOrderingLitOps m = selectValOps isOrdering convert m
  where
    isOrdering :: LHsExpr GhcPs -> Bool
    isOrdering (L _ (HsVar _ (L _ rdr))) = rdrStr rdr `elem` ["GT", "LT", "EQ"]
    isOrdering _ = False

    convert :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    convert (L _ (HsVar _ (L _ rdr))) = case rdrStr rdr of
        "GT" -> [mkDataVar "LT"]
        "LT" -> [mkDataVar "GT"]
        "EQ" -> [mkDataVar "GT", mkDataVar "LT"]
        _    -> []
    convert _ = []
