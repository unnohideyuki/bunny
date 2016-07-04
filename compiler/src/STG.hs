module STG where

import Prelude hiding((<$>))
import Text.PrettyPrint.ANSI.Leijen
import Data.List.Split
import Data.Char
import Data.List

import Symbol
import qualified Core

data Var = TermVar Id
         deriving Show

data Literal = LitStr String
             | LitChar Char
             deriving Show

data Atom = VarAtom Var
          | LitAtom Literal
          deriving Show

data Expr = AtomExpr Atom
          | FunAppExpr Expr [Expr]
          | LetExpr [Bind] Expr
          deriving Show

data Bind = Bind Var Expr
          deriving Show

type Program = [Bind]

{- Todo: (isUpper.head) may not be enough to detect a module name.
         eg. When using Unicode name for module.
-}
isLocal :: Id -> Bool
isLocal s =
  let
    a = dropWhile (isUpper.head) $ splitOn "." s
  in
   (length a > 1) || head s == '_'

-- fv: Free Variables
fv :: Expr -> [Id]

fv (AtomExpr (LitAtom _)) = []

fv (AtomExpr (VarAtom (TermVar n))) =
  case isLocal n of
    True -> [n]
    False -> []

fv (FunAppExpr f args) = fv f ++ concatMap fv args

fv (LetExpr bs e) = (fv e ++ concatMap fv' bs) \\ concatMap bv bs
  where
    fv' (Bind _ e) = fv e
    bv (Bind (TermVar n) _) = [n]

