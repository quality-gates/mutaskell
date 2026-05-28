{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | This module handles the mutation of different patterns.
module Test.MuCheck.Mutation where

import Data.Generics (Typeable, listify, mkMp)
import Data.List (nub, nubBy, partition, permutations, (\\))
import Language.Haskell.Exts (
    Alt (Alt),
    Annotation (Ann),
    Binds (BDecls),
    Decl (AnnPragma, FunBind, PatBind, TypeSig),
    Exp (App, Case, Con, Do, If, InfixApp, Lambda, Let, List, Lit, NegApp, Var),
    Type (TyApp, TyCon, TyFun, TyList),
    Extension (..),
    GuardedRhs (GuardedRhs),
    KnownExtension (..),
    Literal (Char, Frac, Int, PrimChar, PrimDouble, PrimFloat, PrimInt, PrimString, PrimWord, String),
    Match (Match),
    Module (Module),
    ModuleHead (..),
    ModuleName (..),
    Name (Ident, Symbol),
    ParseMode (..),
    Pat (PVar, PWildCard),
    QName (UnQual),
    QOp (QVarOp),
    Rhs (..),
    SrcSpan (..),
    SrcSpanInfo (..),
    Stmt (Generator, LetStmt, Qualifier),
    defaultParseMode,
    fromParseResult,
    parseModuleWithMode,
    prettyPrint,
 )

import Test.MuCheck.Config
import Test.MuCheck.MuOp
import Test.MuCheck.TestAdapter
import Test.MuCheck.Tix
import Test.MuCheck.Utils.Common
import Test.MuCheck.Utils.Syb

{- | The `genMutants` function is a wrapper to genMutantsWith with standard
configuraton
-}
genMutants ::
    -- | The module we are mutating
    FilePath ->
    -- | Coverage information for the module
    FilePath ->
    -- | Returns the covering mutants produced, and original length.
    IO (Int, [Mutant])
genMutants = genMutantsWith defaultConfig

{- | The `genMutantsWith` function takes configuration function to mutate,
function to mutate, filename the function is defined in, and produces
mutants in the same directory as the filename, and returns the number
of mutants produced.
-}
genMutantsWith ::
    -- | The configuration to be used
    Config ->
    -- | The module we are mutating
    FilePath ->
    -- | Coverage information for the module
    FilePath ->
    -- | Returns the covered mutants produced, and the original number
    IO (Int, [Mutant])
genMutantsWith _config filename tix = do
    f <- readFile filename

    let origAst = getASTFromStr f
        modul = getModuleName origAst
        mutants :: [Mutant]
        mutants = genMutantsFromAST defaultConfig origAst

    -- We have a choice here. We could allow users to specify test specific
    -- coverage rather than a single coverage. This can further reduce the
    -- mutants.
    c <- getUnCoveredPatches tix modul
    -- check if the mutants span is within any of the covered spans.
    return $ case c of
        Nothing -> (-1, mutants)
        Just v -> (length mutants, removeUncovered v mutants)

-- | Remove mutants that are not covered by any tests
removeUncovered :: [Span] -> [Mutant] -> [Mutant]
removeUncovered uspans mutants = filter isMCovered mutants -- get only covering mutants.
  where
    isMCovered :: Mutant -> Bool
    -- \| is it contained in any of the spans? if it is, then return false.
    isMCovered Mutant{..} = not $ any (insideSpan _mspan) uspans

-- | Get the module name from ast
getModuleName :: Module t -> String
getModuleName (Module _ (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ _) = name
getModuleName _ = ""

{- | The `genMutantsForSrc` takes the function name to mutate, source where it
is defined, and returns the mutated sources
-}
genMutantsForSrc ::
    -- | Configuration
    Config ->
    -- | The module we are mutating
    String ->
    -- | Returns the mutants
    [Mutant]
genMutantsForSrc config src = genMutantsFromAST config (getASTFromStr src)

{- | Like 'genMutantsForSrc' but accepts a pre-parsed AST, avoiding a second
parse when the caller already has one.
-}
genMutantsFromAST ::
    -- | Configuration
    Config ->
    -- | Pre-parsed AST of the module to mutate
    Module_ ->
    -- | Returns the mutants
    [Mutant]
genMutantsFromAST config origAst =
    nubBy (\a b -> _mutant a == _mutant b) $
        filter (\m -> _mutant m /= origStr) $
            map (toMutant . apTh (prettyPrint . withAnn)) $
                programMutants config ast
  where
    (onlyAnn, noAnn) = splitAnnotations origAst
    ast = putDecl origAst noAnn
    withAnn mast = putDecl mast $ getDecl mast ++ onlyAnn
    origStr = prettyPrint (withAnn ast)

-- | Produce all mutants after applying all operators
programMutants ::
    -- | Configuration
    Config ->
    -- | Module to mutate
    Module_ ->
    -- | Returns mutated modules
    [(MuVar, Span, Module_)]
programMutants config ast = nub $ mutatesN (applicableOps config ast) ast fstOrder
  where
    fstOrder = 1 -- first order

-- | Returns all mutation operators
applicableOps ::
    -- | Configuration
    Config ->
    -- | Module to mutate
    Module_ ->
    -- | Returns mutation operators
    [(MuVar, MuOp)]
applicableOps config ast = relevantOps ast opsList
  where
    opsList =
        concatMap
            spread
            [ (MutatePatternMatch, selectFnMatches ast)
            , (MutateValues, selectLiteralOps ast)
            , (MutateFunctions, selectFunctionOps (muOp config) ast)
            , (MutateNegateIfElse, selectIfElseBoolNegOps ast)
            , (MutateNegateGuards, selectGuardedBoolNegOps ast)
            , (MutateOther "remove-not", selectRemoveNotOps ast)
            , (MutateOther "remove-negation", selectRemoveNegationOps ast)
            , (MutateOther "case-alt-remove", selectCaseAltRemoveOps ast)
            , (MutateOther "case-default-remove", selectCaseDefaultRemoveOps ast)
            , (MutateOther "remove-stmt", selectRemoveStmtOps ast)
            , (MutateOther "remove-let-binding", selectRemoveLetBindingOps ast)
            , (MutateOther "remove-where-binding", selectRemoveWhereBindingOps ast)
            , (MutateOther "remove-self-assign", selectRemoveSelfAssignOps ast)
            , (MutateOther "negate-literal", selectNegateLiteralOps ast)
            , (MutateOther "string-literal", selectStringLiteralOps ast)
            , (MutateOther "bool-operand", selectBoolOperandOps ast)
            , (MutateOther "flip-maybe", selectFlipMaybeOps ast)
            , (MutateOther "flip-either", selectFlipEitherOps ast)
            , (MutateOther "remove-forkIO", selectRemoveForkIOOps ast)
            , (MutateOther "bracket-degenerate", selectBracketDegenerateOps ast)
            , (MutateOther "error-guard", selectErrorGuardOps ast)
            , (MutateOther "replace-mutable-arg", selectReplaceMutableArgOps ast)
            , (MutateOther "zero-return", selectZeroReturnOps ast)
            ]

-- | Split declarations of the module to annotated and non annotated.
splitAnnotations :: Module_ -> ([Decl_], [Decl_])
splitAnnotations ast = partition fn $ getDecl ast
  where
    fn x = (functionName x ++ pragmaName x) `elem` getAnnotatedTests ast

-- only one of pragmaName or functionName will be present at a time.

-- | Returns the annotated tests and their annotations
getAnnotatedTests :: Module_ -> [String]
getAnnotatedTests ast = concatMap (getAnn ast) ["Test", "TestSupport"]

-- | Get the embedded declarations from a module.
getDecl :: Module_ -> [Decl_]
getDecl (Module _ _ _ _ decls) = decls
getDecl _ = []

-- | Put the given declarations into the given module
putDecl :: Module_ -> [Decl_] -> Module_
putDecl (Module a b c d _) decls = Module a b c d decls
putDecl m _ = m

{- | First and higher order mutation. The actual apply of mutation operators,
and generation of mutants happens here.
The third argument specifies whether it's first order or higher order
-}
mutatesN ::
    -- | Applicable Operators
    [(MuVar, MuOp)] ->
    -- | Module to mutate
    Module_ ->
    -- | Order of mutation (usually 1 - first order)
    Int ->
    -- | Returns the mutated module
    [(MuVar, Span, Module_)]
mutatesN os ast n = mutatesN' os (MutateOther [], toSpan (0, 0, 0, 0), ast) n
  where
    mutatesN' ops ms 1 = concat [mutate op ms | op <- ops]
    mutatesN' ops ms c = concat [mutatesN' ops m 1 | m <- mutatesN' ops ms $ pred c]

{- | Given a function, generate all mutants after applying applying
op once (op might be applied at different places).
E.g.: if the operator is (op = "<" ==> ">") and there are two instances of
"<" in the AST, then it will return two AST with each replaced.
-}
mutate :: (MuVar, MuOp) -> (MuVar, Span, Module_) -> [(MuVar, Span, Module_)]
mutate (v, op) (_v, _s, m) = map (v,toSpan $ getSpan op,) $ once (mkMpMuOp op) m \\ [m]

{- | Generate sub-arrays with one less element except when we have only
a single element.
-}
removeOneElem :: (Eq t) => [t] -> [[t]]
removeOneElem [_] = []
removeOneElem l = choose l (length l - 1)

-- AST/module-related operations

-- | Returns the AST from the file
getASTFromStr :: String -> Module_
getASTFromStr fname = fromParseResult $ parseModuleWithMode mode fname
  where
    mode = defaultParseMode{extensions = exts}
    exts =
        [ EnableExtension ScopedTypeVariables
        , EnableExtension MultiParamTypeClasses
        , EnableExtension FunctionalDependencies
        , EnableExtension FlexibleInstances
        , EnableExtension FlexibleContexts
        , EnableExtension TypeFamilies
        , EnableExtension GADTs
        ]

-- | get all annotated functions
getAnn :: Module_ -> String -> [String]
getAnn m s = [conv name | Ann _l name _exp <- listify isAnn m]
  where
    isAnn :: Annotation_ -> Bool
    isAnn (Ann _l (Symbol _lsy _name) (Lit _ll (String _ls e _))) = e == s
    isAnn (Ann _l (Ident _lid _name) (Lit _ll (String _ls e _))) = e == s
    isAnn _ = False
    conv (Symbol _l n) = n
    conv (Ident _l n) = n

-- | given the module name, return all marked tests
getAllTests :: String -> IO [String]
getAllTests modname = allTests <$> readFile modname

-- | Given module source, return all marked tests
allTests :: String -> [String]
allTests modsrc = getAnn (getASTFromStr modsrc) "Test"

-- | The name of a function
functionName :: Decl_ -> String
functionName (FunBind _l (Match _ (Ident _li n) _ _ _ : _)) = n
functionName (FunBind _l (Match _ (Symbol _ls n) _ _ _ : _)) = n
-- we also consider where clauses
functionName (PatBind _ (PVar _lpv (Ident _li n)) _ _) = n
functionName _ = []

-- | The identifier of declared pragma
pragmaName :: Decl_ -> String
pragmaName (AnnPragma _ (Ann _l (Ident _li n) (Lit _ll (String _ls _t _)))) = n
pragmaName _ = []

-- but not let, because it has a different type, and for our purposes
-- this is sufficient.
-- (Let Binds Exp) :: Exp

{- | For valops, we specify how any given literal value might
change. So we take a predicate specifying how to recognize the literal
value, a list of mappings specifying how the literal can change, and the
AST, and recurse over the AST looking for literals that match our predicate.
When we find any, we apply the given list of mappings to them, and produce
a MuOp mapping between the original value and transformed value. This list
of MuOp mappings are then returned.
-}
selectValOps :: (Typeable b, Mutable b) => (b -> Bool) -> (b -> [b]) -> Module_ -> [MuOp]
selectValOps predicate f m = concat [x ==>* f x | x <- vals]
  where
    vals = listify predicate m

-- | Look for literal values in AST, and return applicable MuOp transforms.
selectLiteralOps :: Module_ -> [MuOp]
selectLiteralOps m = selectLitOps m ++ selectBLitOps m

{- | Look for literal values in AST, and return applicable MuOp transforms.
Unfortunately booleans are not handled here.
-}
selectLitOps :: Module_ -> [MuOp]
selectLitOps m = selectValOps isLit convert m
  where isLit :: Literal_ -> Bool
        isLit Int{} = True
        isLit PrimInt{} = True
        isLit Char{} = True
        isLit PrimChar{} = True
        isLit Frac{} = True
        isLit PrimFloat{} = True
        isLit PrimDouble{} = True
        isLit String{} = True
        isLit PrimString{} = True
        isLit PrimWord{} = True
        convert (Int l i _) = map (apX (Int l)) $ nub [i + 1, i - 1, 0, 1]
        convert (PrimInt l i _) = map (apX (PrimInt l)) $ nub [i + 1, i - 1, 0, 1]
        convert (Char l c _) = map (apX (Char l)) [pred c, succ c]
        convert (PrimChar l c _) = map (apX (PrimChar l)) [pred c, succ c]
        convert (Frac l f _) = map (apX (Frac l)) $ nub [f + 1.0, f - 1.0, 0.0, 1.1]
        convert (PrimFloat l f _) = map (apX (PrimFloat l)) $ nub [f + 1.0, f - 1.0, 0.0, 1.0]
        convert (PrimDouble l f _) = map (apX (PrimDouble l)) $ nub [f + 1.0, f - 1.0, 0.0, 1.0]
        convert (String l _ _) = map (apX (String l)) $ nub [""]
        convert (PrimString l _ _) = map (apX (PrimString l)) $ nub [""]
        convert (PrimWord l i _) = map (apX (PrimWord l)) $ nub [i + 1, i - 1, 0, 1]
        apX :: (t1 -> [a] -> t) -> t1 -> t
        apX fn i = fn i []

{- | Convert Boolean Literals

> (True, False)

becomes

> (False, True)
-}
selectBLitOps :: Module_ -> [MuOp]
selectBLitOps m = selectValOps isLit convert m
  where
    isLit :: Name_ -> Bool
    isLit (Ident _l "True") = True
    isLit (Ident _l "False") = True
    isLit _ = False
    convert (Ident l "True") = [Ident l "False"]
    convert (Ident l "False") = [Ident l "True"]
    convert _ = []

{- | Negating boolean in if/else statements

> if True then 1 else 0

becomes

> if True then 0 else 1
-}
selectIfElseBoolNegOps :: Module_ -> [MuOp]
selectIfElseBoolNegOps m = selectValOps isIf convert m
  where
    isIf :: Exp_ -> Bool
    isIf If{} = True
    isIf _ = False
    convert (If l e1 e2 e3) = [If l e1 e3 e2]
    convert _ = []

{- | Negating boolean in Guards
| negate guarded booleans in guarded definitions

> myFn x | x == 1 = True
> myFn   | otherwise = False

becomes

> myFn x | not (x == 1) = True
> myFn   | otherwise = False
-}
selectGuardedBoolNegOps :: Module_ -> [MuOp]
selectGuardedBoolNegOps m = selectValOps isGuardedRhs convert m
  where
    isGuardedRhs :: GuardedRhs_ -> Bool
    isGuardedRhs GuardedRhs{} = True
    convert (GuardedRhs l stmts expr) = [GuardedRhs l s expr | s <- once (mkMp boolNegate) stmts]
    boolNegate _e@(Qualifier _l (Var _lv (UnQual _lu (Ident _li "otherwise")))) = [] -- VERIFY
    boolNegate (Qualifier l expr) = [Qualifier l (App l_ (Var l_ (UnQual l_ (Ident l_ "not"))) expr)]
    boolNegate _x = [] -- VERIFY

-- | dummy
l_ :: SrcSpanInfo
l_ = SrcSpanInfo (SrcSpan "" 0 0 0 0) []

{- | Generate all operators for permuting and removal of pattern guards from
function definitions

> myFn (x:xs) = False
> myFn _ = True

becomes

> myFn _ = True
> myFn (x:xs) = False

> myFn _ = True

> myFn (x:xs) = False
-}
selectFnMatches :: Module_ -> [MuOp]
selectFnMatches m = selectValOps isFunct convert m
  where
    isFunct :: Decl_ -> Bool
    isFunct FunBind{} = True
    isFunct _ = False
    convert (FunBind l ms) = map (FunBind l) $ filter (/= ms) (permutations ms ++ removeOneElem ms)
    convert _ = []

{- | Generate all operators for permuting symbols like binary operators
Since we are looking for symbols, we are reasonably sure that it is not
locally bound to a variable.
-}
selectSymbolFnOps :: Module_ -> [String] -> [MuOp]
selectSymbolFnOps m s = selectValOps isBin convert m
  where
    isBin :: Name_ -> Bool
    isBin (Symbol _l n) | n `elem` s = True
    isBin _ = False
    convert (Symbol l n) = map (Symbol l) $ filter (/= n) s
    convert _ = []

{- | Generate all operators for permuting commonly used functions (with
identifiers).
-}
selectIdentFnOps :: Module_ -> [String] -> [MuOp]
selectIdentFnOps m s = selectValOps isCommonFn convert m
  where
    isCommonFn :: Exp_ -> Bool
    isCommonFn (Var _lv (UnQual _lu (Ident _l n))) | n `elem` s = True
    isCommonFn _ = False
    convert (Var lv_ (UnQual lu_ (Ident li_ n))) = map (Var lv_ . UnQual lu_ . Ident li_) $ filter (/= n) s
    convert _ = []

-- | Generate all operators depending on whether it is a symbol or not.
selectFunctionOps :: [FnOp] -> Module_ -> [MuOp]
selectFunctionOps fo f = concatMap (selectIdentFnOps f) idents ++ concatMap (selectSymbolFnOps f) syms
  where
    idents = map _fns $ filter (\a -> _type a == FnIdent) fo
    syms = map _fns $ filter (\a -> _type a == FnSymbol) fo

-- (Var l (UnQual l (Ident l "ab")))
-- (App l (Var l (UnQual l (Ident l "head"))) (Var l (UnQual l (Ident l "b"))))
-- (App l (App l (Var l (UnQual l (Ident l "head"))) (Var l (UnQual l (Ident l "a")))) (Var l (UnQual l (Ident l "b")))))
-- (InfixApp l (Var l (UnQual l (Ident l "a"))) (QVarOp l (UnQual l (Symbol l ">"))) (Var l (UnQual l (Ident l "b"))))
-- (InfixApp l (Var l (UnQual l (Ident l "a"))) (QVarOp l (UnQual l (Ident l "x"))) (Var l (UnQual l (Ident l "b"))))

-- | Remove 'not' application: replace @not expr@ with @expr@
selectRemoveNotOps :: Module_ -> [MuOp]
selectRemoveNotOps m = selectValOps isNotApp removeNot m
  where
    isNotApp :: Exp_ -> Bool
    isNotApp (App _ (Var _ (UnQual _ (Ident _ "not"))) _) = True
    isNotApp _ = False
    removeNot (App _ _ expr) = [expr]
    removeNot _ = []

-- | Remove negation: replace @negate expr@ or @-expr@ with @expr@
selectRemoveNegationOps :: Module_ -> [MuOp]
selectRemoveNegationOps m = selectValOps isNeg removeNeg m
  where
    isNeg :: Exp_ -> Bool
    isNeg (NegApp _ _) = True
    isNeg (App _ (Var _ (UnQual _ (Ident _ "negate"))) _) = True
    isNeg _ = False
    removeNeg (NegApp _ expr) = [expr]
    removeNeg (App _ _ expr) = [expr]
    removeNeg _ = []

-- | Remove one alternative from 'case...of'
selectCaseAltRemoveOps :: Module_ -> [MuOp]
selectCaseAltRemoveOps m = selectValOps isCase convert m
  where
    isCase :: Exp_ -> Bool
    isCase (Case _ _ alts) = length alts > 1
    isCase _ = False
    convert :: Exp_ -> [Exp_]
    convert (Case l e alts) = [Case l e alts' | alts' <- removeOneElem alts]
    convert _ = []

-- | Remove default alternative from 'case...of' and 'otherwise' from guards
selectCaseDefaultRemoveOps :: Module_ -> [MuOp]
selectCaseDefaultRemoveOps m = selectCaseAltDefaultOps m ++ selectGuardedDefaultOps m
  where
    selectCaseAltDefaultOps :: Module_ -> [MuOp]
    selectCaseAltDefaultOps m' = selectValOps isCase convertAlt m'
    isCase :: Exp_ -> Bool
    isCase (Case _ _ alts) = any isDefault alts && length alts > 1
    isCase _ = False
    isDefault :: Alt_ -> Bool
    isDefault (Alt _ (PWildCard _) _ _) = True
    isDefault (Alt _ (PVar _ (Ident _ "otherwise")) _ _) = True
    isDefault _ = False
    convertAlt :: Exp_ -> [Exp_]
    convertAlt (Case l e alts) = [Case l e (filter (not . isDefault) alts)]
    convertAlt _ = []

    selectGuardedDefaultOps :: Module_ -> [MuOp]
    selectGuardedDefaultOps m' = selectValOps isRhs convertRhs m'
    isRhs :: Rhs_ -> Bool
    isRhs (GuardedRhss _ grhss) = any isDefaultGuardedRhs grhss && length grhss > 1
    isRhs _ = False
    isDefaultGuardedRhs :: GuardedRhs_ -> Bool
    isDefaultGuardedRhs (GuardedRhs _ stmts _) = any isDefaultStmt stmts
    isDefaultStmt :: Stmt_ -> Bool
    isDefaultStmt (Qualifier _ (Var _ (UnQual _ (Ident _ "otherwise")))) = True
    isDefaultStmt _ = False
    convertRhs :: Rhs_ -> [Rhs_]
    convertRhs (GuardedRhss l grhss) = [GuardedRhss l (filter (not . isDefaultGuardedRhs) grhss)]
    convertRhs _ = []

-- | Remove one statement from 'do' block
selectRemoveStmtOps :: Module_ -> [MuOp]
selectRemoveStmtOps m = selectValOps isDo convert m
  where
    isDo :: Exp_ -> Bool
    isDo Do{} = True
    isDo _ = False
    convert :: Exp_ -> [Exp_]
    convert (Do l stmts) = [Do l stmts' | stmts' <- removeOneStmt stmts]
    convert _ = []
    removeOneStmt :: [Stmt_] -> [[Stmt_]]
    removeOneStmt stmts =
        [ take i stmts ++ drop (i + 1) stmts
        | i <- [0 .. length stmts - 1]
        , isValidDo (take i stmts ++ drop (i + 1) stmts)
        ]
    -- A do block must end with an expression (Qualifier)
    isValidDo :: [Stmt_] -> Bool
    isValidDo [] = False
    isValidDo stmts = case last stmts of
        Qualifier{} -> True
        _ -> False

-- | Remove one binding from 'let...in' and 'do' block 'let'
selectRemoveLetBindingOps :: Module_ -> [MuOp]
selectRemoveLetBindingOps m = selectValOps isLet convertLet m ++ selectValOps isDo convertDo m
  where
    isLet :: Exp_ -> Bool
    isLet (Let _ (BDecls _ decls) _) = length decls > 0
    isLet _ = False
    convertLet :: Exp_ -> [Exp_]
    convertLet (Let l (BDecls lb decls) e) = [Let l (BDecls lb decls') e | decls' <- removeOneElem decls]
    convertLet _ = []

    isDo (Do _ stmts) = any isLetStmt stmts
    isDo _ = False
    isLetStmt :: Stmt_ -> Bool
    isLetStmt (LetStmt _ (BDecls _ decls)) = not (null decls)
    isLetStmt _ = False

    convertDo :: Exp_ -> [Exp_]
    convertDo (Do l stmts) =
        [ Do l (replaceAt i s' stmts)
        | (i, s) <- zip [0 ..] stmts
        , s' <- convertLetStmt s
        ]
    convertDo _ = []

    convertLetStmt :: Stmt_ -> [Stmt_]
    convertLetStmt (LetStmt l (BDecls lb decls)) = [LetStmt l (BDecls lb decls') | decls' <- removeOneElem decls]
    convertLetStmt _ = []

-- | Remove one binding from 'where' clauses
selectRemoveWhereBindingOps :: Module_ -> [MuOp]
selectRemoveWhereBindingOps m = selectValOps isFunBind convertFunBind m ++ selectValOps isPatBind convertPat m
  where
    isFunBind :: Decl_ -> Bool
    isFunBind (FunBind _ matches) = any hasBinds matches
    isFunBind _ = False
    hasBinds (Match _ _ _ _ (Just (BDecls _ decls))) = length decls > 0
    hasBinds _ = False

    convertFunBind :: Decl_ -> [Decl_]
    convertFunBind (FunBind l matches) =
        [ FunBind l (replaceAt i m' matches)
        | (i, m_) <- zip [0 ..] matches
        , m' <- convertMatch m_
        ]
    convertFunBind _ = []

    convertMatch :: Match SrcSpanInfo -> [Match SrcSpanInfo]
    convertMatch (Match l n p r (Just (BDecls lb decls))) = [Match l n p r (Just (BDecls lb decls')) | decls' <- removeOneElem decls]
    convertMatch _ = []

    isPatBind :: Decl_ -> Bool
    isPatBind (PatBind _ _ _ (Just (BDecls _ decls))) = length decls > 0
    isPatBind _ = False
    convertPat :: Decl_ -> [Decl_]
    convertPat (PatBind l p r (Just (BDecls lb decls))) = [PatBind l p r (Just (BDecls lb decls')) | decls' <- removeOneElem decls]
    convertPat _ = []

-- | Replace element at given index in a list
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- | Remove self assignments: @let x = x@ and @x <- return x@
selectRemoveSelfAssignOps :: Module_ -> [MuOp]
selectRemoveSelfAssignOps m = selectValOps isLet convertLet m ++ selectValOps isDo convertDo m
  where
    isLet (Let _ (BDecls _ decls) _) = any isSelfAssign decls
    isLet _ = False
    isSelfAssign (PatBind _ (PVar _ (Ident _ x)) (UnGuardedRhs _ (Var _ (UnQual _ (Ident _ x2)))) Nothing) = x == x2
    isSelfAssign _ = False
    convertLet :: Exp_ -> [Exp_]
    convertLet (Let l (BDecls lb decls) e) = [Let l (BDecls lb (filter (not . isSelfAssign) decls)) e]
    convertLet _ = []

    isDo (Do _ stmts) = any isSelfAssignStmt stmts
    isDo _ = False
    isSelfAssignStmt :: Stmt_ -> Bool
    isSelfAssignStmt (Generator _ (PVar _ (Ident _ x)) (App _ (Var _ (UnQual _ (Ident _ "return"))) (Var _ (UnQual _ (Ident _ x2))))) = x == x2
    isSelfAssignStmt _ = False
    convertDo :: Exp_ -> [Exp_]
    convertDo (Do l stmts) = [Do l (filter (not . isSelfAssignStmt) stmts)]
    convertDo _ = []

-- | Negate numeric literals: @42@ becomes @negate 42@
selectNegateLiteralOps :: Module_ -> [MuOp]
selectNegateLiteralOps m = selectValOps isPosLit convert m
  where
    isPosLit (Lit _ (Int _ i _)) | i > 0 = True
    isPosLit (Lit _ (Frac _ f _)) | f > 0 = True
    isPosLit _ = False
    convert :: Exp_ -> [Exp_]
    convert (Lit l (Int _ i s)) = [App l (Var l (UnQual l (Ident l "negate"))) (Lit l (Int l i s))]
    convert (Lit l (Frac _ f s)) = [App l (Var l (UnQual l (Ident l "negate"))) (Lit l (Frac l f s))]
    convert _ = []

-- | Replace string literals in comparisons and guards with @""@
selectStringLiteralOps :: Module_ -> [MuOp]
selectStringLiteralOps m = selectValOps isStringComp convert m
  where
    isStringComp (InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ op))) e2)
        | op `elem` ["==", "/="] = isNonEmptyString e1 || isNonEmptyString e2
    isStringComp _ = False
    isNonEmptyString (Lit _ (String _ s _)) = not (null s)
    isNonEmptyString _ = False
    convert :: Exp_ -> [Exp_]
    convert (InfixApp l e1 op e2) =
        [InfixApp l (if isNonEmptyString e1 then emptyStr else e1) op (if isNonEmptyString e2 then emptyStr else e2)]
    convert _ = []
    emptyStr = Lit l_ (String l_ "" "")

-- | Replace operands in @&&@ and @||@ with @True@ or @False@
selectBoolOperandOps :: Module_ -> [MuOp]
selectBoolOperandOps m = selectValOps isBoolOp convert m
  where
    isBoolOp (InfixApp _ _ (QVarOp _ (UnQual _ (Symbol _ op))) _) = op `elem` ["&&", "||"]
    isBoolOp _ = False
    convert :: Exp_ -> [Exp_]
    convert (InfixApp l e1 op e2) =
        [ InfixApp l (Var l (UnQual l (Ident l "True"))) op e2
        , InfixApp l (Var l (UnQual l (Ident l "False"))) op e2
        , InfixApp l e1 op (Var l (UnQual l (Ident l "True")))
        , InfixApp l e1 op (Var l (UnQual l (Ident l "False")))
        ]
    convert _ = []

-- | Flip @Maybe@ values: @Just x@ <-> @Nothing@
selectFlipMaybeOps :: Module_ -> [MuOp]
selectFlipMaybeOps m = selectValOps isMaybe convert m
  where
    isMaybe (App _ (Con _ (UnQual _ (Ident _ "Just"))) _) = True
    isMaybe (Con _ (UnQual _ (Ident _ "Nothing"))) = True
    isMaybe _ = False
    convert :: Exp_ -> [Exp_]
    convert (App l (Con _ (UnQual _ (Ident _ "Just"))) _) = [Con l (UnQual l (Ident l "Nothing"))]
    convert (Con l (UnQual _ (Ident _ "Nothing"))) = [App l (Con l (UnQual l (Ident l "Just"))) (Var l (UnQual l (Ident l "undefined")))]
    convert _ = []

-- | Flip @Either@ values: @Right x@ <-> @Left x@
selectFlipEitherOps :: Module_ -> [MuOp]
selectFlipEitherOps m = selectValOps isEither convert m
  where
    isEither (App _ (Con _ (UnQual _ (Ident _ "Right"))) _) = True
    isEither (App _ (Con _ (UnQual _ (Ident _ "Left"))) _) = True
    isEither (Con _ (UnQual _ (Ident _ "Right"))) = True
    isEither (Con _ (UnQual _ (Ident _ "Left"))) = True
    isEither _ = False
    convert :: Exp_ -> [Exp_]
    convert (App l (Con _ (UnQual _ (Ident _ "Right"))) e) = [App l (Con l (UnQual l (Ident l "Left"))) e]
    convert (App l (Con _ (UnQual _ (Ident _ "Left"))) e) = [App l (Con l (UnQual l (Ident l "Right"))) e]
    convert (Con l (UnQual _ (Ident _ "Right"))) = [Con l (UnQual l (Ident l "Left"))]
    convert (Con l (UnQual _ (Ident _ "Left"))) = [Con l (UnQual l (Ident l "Right"))]
    convert _ = []

-- | Remove @forkIO@, @async@, @withAsync@: run action inline
selectRemoveForkIOOps :: Module_ -> [MuOp]
selectRemoveForkIOOps m = selectValOps isFork convert m
  where
    isFork (App _ (Var _ (UnQual _ (Ident _ "forkIO"))) _) = True
    isFork (App _ (Var _ (UnQual _ (Ident _ "async"))) _) = True
    isFork (App _ (App _ (Var _ (UnQual _ (Ident _ "withAsync"))) _) _) = True
    isFork _ = False
    convert :: Exp_ -> [Exp_]
    convert (App _ (Var _ (UnQual _ (Ident _ "forkIO"))) e) = [e]
    convert (App _ (Var _ (UnQual _ (Ident _ "async"))) e) = [e]
    convert (App l (App _ (Var _ (UnQual _ (Ident _ "withAsync"))) e1) (Lambda _ [PVar _ _] e2)) =
        [InfixApp l e1 (QVarOp l (UnQual l (Symbol l ">>"))) e2]
    convert (App l (App _ (Var _ (UnQual _ (Ident _ "withAsync"))) e1) e2) =
        [InfixApp l e1 (QVarOp l (UnQual l (Symbol l ">>"))) e2]
    convert _ = []

-- | Replace @bracket acquire release action@ with @acquire >>= action@
selectBracketDegenerateOps :: Module_ -> [MuOp]
selectBracketDegenerateOps m = selectValOps isBracket convert m
  where
    isBracket (App _ (App _ (App _ (Var _ (UnQual _ (Ident _ "bracket"))) _) _) _) = True
    isBracket _ = False
    convert :: Exp_ -> [Exp_]
    convert (App l (App _ (App _ (Var _ (UnQual _ (Ident _ "bracket"))) e1) _) e3) =
        [InfixApp l e1 (QVarOp l (UnQual l (Symbol l ">>="))) e3]
    convert _ = []

-- | Replace exception handlers with a no-op returning @undefined@
selectErrorGuardOps :: Module_ -> [MuOp]
selectErrorGuardOps m = selectValOps isErrorOp convert m
  where
    isErrorOp (App _ (App _ (Var _ (UnQual _ (Ident _ "catch"))) _) _) = True
    isErrorOp (App _ (App _ (Var _ (UnQual _ (Ident _ "handle"))) _) _) = True
    isErrorOp (App _ (Var _ (UnQual _ (Ident _ "try"))) _) = True
    isErrorOp _ = False
    convert :: Exp_ -> [Exp_]
    convert (App l (App _ (Var _ (UnQual _ (Ident _ "catch"))) e1) _) =
        [App l (App l (Var l (UnQual l (Ident l "catch"))) e1) (Lambda l [PWildCard l] (App l (Var l (UnQual l (Ident l "return"))) (Var l (UnQual l (Ident l "undefined")))))]
    convert (App l (App _ (Var _ (UnQual _ (Ident _ "handle"))) _) e2) =
        [App l (Lambda l [PWildCard l] (App l (Var l (UnQual l (Ident l "return"))) (Var l (UnQual l (Ident l "undefined"))))) e2]
    convert (App l (Var _ (UnQual _ (Ident _ "try"))) e) =
        [App l (Var l (UnQual l (Ident l "return"))) (App l (Con l (UnQual l (Ident l "Right"))) e)]
    convert _ = []

-- | Replace @IORef@/@MVar@/@TVar@ arguments with @undefined@
selectReplaceMutableArgOps :: Module_ -> [MuOp]
selectReplaceMutableArgOps m = selectValOps isMutableVar convert m
  where isMutableVar (Var _ (UnQual _ (Ident _ n))) | n `elem` ["ref", "mvar", "tvar", "ior", "stref"] = True
        isMutableVar _ = False
        convert :: Exp_ -> [Exp_]
        convert (Var l _) = [Var l (UnQual l (Ident l "undefined"))]
        convert _ = []

-- | Replace each function match body with the zero value for the declared return type.
-- Only applies to functions that have a type signature in the same module.
selectZeroReturnOps :: Module_ -> [MuOp]
selectZeroReturnOps (Module _ _ _ _ decls) =
    [ rhs ==> UnGuardedRhs l_ zv
    | FunBind _ matches <- decls
    , Match _ name _ rhs _ <- matches
    , Just retType <- [lookup (nameStr name) typeSigs]
    , Just zv <- [typeZeroVal retType]
    ]
  where
    typeSigs = [(nameStr n, returnType t) | TypeSig _ ns t <- decls, n <- ns]
    nameStr (Ident _ s) = s
    nameStr (Symbol _ s) = s
    returnType (TyFun _ _ t) = returnType t
    returnType t = t
    typeZeroVal (TyCon _ (UnQual _ (Ident _ "Bool")))    = Just $ Var l_ (UnQual l_ (Ident l_ "False"))
    typeZeroVal (TyCon _ (UnQual _ (Ident _ "Int")))     = Just $ Lit l_ (Int l_ 0 "0")
    typeZeroVal (TyCon _ (UnQual _ (Ident _ "Integer"))) = Just $ Lit l_ (Int l_ 0 "0")
    typeZeroVal (TyCon _ (UnQual _ (Ident _ "Double")))  = Just $ Lit l_ (Frac l_ 0 "0.0")
    typeZeroVal (TyCon _ (UnQual _ (Ident _ "Float")))   = Just $ Lit l_ (Frac l_ 0 "0.0")
    typeZeroVal (TyCon _ (UnQual _ (Ident _ "String")))  = Just $ Lit l_ (String l_ "" "\"\"")
    typeZeroVal (TyList _ _)                              = Just $ List l_ []
    typeZeroVal (TyApp _ (TyCon _ (UnQual _ (Ident _ "Maybe"))) _) = Just $ Var l_ (UnQual l_ (Ident l_ "Nothing"))
    typeZeroVal (TyApp _ (TyCon _ (UnQual _ (Ident _ "IO"))) _) =
      Just $ App l_ (Var l_ (UnQual l_ (Ident l_ "return"))) (Var l_ (UnQual l_ (Ident l_ "undefined")))
    typeZeroVal _ = Nothing
selectZeroReturnOps _ = []
