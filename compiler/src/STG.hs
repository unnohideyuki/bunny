module STG where

import           Symbol

import           Data.Char
import           Data.List
import           Data.List.Split
-- import           Debug.Trace

data Var = TermVar Id
         | DictVar Id Id
         | CompositDict Expr [Expr]
         deriving Show

data Literal = LitStr String
             | LitChar Char
             | LitInt Integer
             | LitFrac Double
             deriving Show

data Atom = VarAtom Var
          | LitAtom Literal
          deriving Show

data Expr = AtomExpr Atom
          | FunAppExpr Expr [Expr]
          | LetExpr [Bind] Expr
          | LamExpr [Var] Expr
          | CaseExpr Expr [Alt] {- CaseExpr Expr Var [Alt] -}
          deriving Show

data Bind = Bind Var Expr
          deriving Show

data Alt = CotrAlt Id Expr
         | DefaultAlt Expr
         deriving Show

type Program = [Bind]

{- Todo: (isUpper.head) may not be enough to detect a module name.
         eg. When using Unicode name for module.
-}
isLocal :: Id -> Bool
isLocal s =
  let
    -- isLocal "Main.." will error and the last '.' should be replaced
    s' = case last s of
      '.' -> take (length s - 1) s ++ "dot"
      _   -> s
    a = dropWhile (isUpper.head) $ splitOn "." s'
  in
   (length a > 1) || head s == '_'

-- fv: Free Variables
fv :: Expr -> [Id]

fv (AtomExpr (LitAtom _)) = []

fv (AtomExpr (VarAtom (TermVar n))) = [n | isLocal n]

fv (AtomExpr (VarAtom (DictVar _ _))) = []

fv (AtomExpr (VarAtom (CompositDict _ _))) = []

fv (FunAppExpr f args) = nub (fv f ++ concatMap fv args)

fv (LetExpr bs e) = nub (fv e ++ concatMap fv' bs) \\ nub (concatMap bv bs)
  where
    fv' (Bind _ e') = fv e'
    bv (Bind (TermVar n) _) = [n]
    bv _                    = error "bv: not expected"

fv (LamExpr vs e) = nub (fv e) \\ nub (map (\(TermVar n) -> n) vs)

fv (CaseExpr scrut alts) = fv scrut `union` fvalts alts []
  where
    fvalts [] xs                   = xs
    fvalts (CotrAlt _ e:alts') xs  = fvalts alts' (fv e `union` xs)
    fvalts (DefaultAlt e:alts') xs = fvalts alts' (fv e `union` xs)

