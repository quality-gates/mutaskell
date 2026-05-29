-- | Helper module for easier visualization
module Test.Muskell.Utils.Helpers where

import GHC.Utils.Outputable (showSDocUnsafe, ppr)
import Test.Muskell.MuOp

-- | Class to allow easier visualization of values without munging @show@
class Showx a where
    showx :: a -> String

-- | Temporary holder for easier visualization
data X
    = ModuleX Module_
    | DeclX   Decl_
    | DeclXs  [Decl_]

-- | 'Showx' instances using GHC's 'Outputable' pretty-printer
instance Showx X where
    showx (ModuleX m)    = "{ " ++ showSDocUnsafe (ppr m) ++ " }\n"
    showx (DeclX  d)     = "{ " ++ showSDocUnsafe (ppr d) ++ " }\n"
    showx (DeclXs decls) = unlines $ map (showx . DeclX) decls
