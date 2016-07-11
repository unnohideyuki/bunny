module Core where

import Prelude hiding ((<$>))

import Symbol
import Text.PrettyPrint.ANSI.Leijen

data Module = Module Id [Bind] {- [Axiom] -}

data Var = TermVar Id Type
         | TypeVar Id Kind
         deriving (Show, Eq)

data Kind = Star | Kfun Kind Kind
          deriving (Show, Eq)

data TyCon = TyCon Id Kind
           deriving (Show, Eq)

data Type = TyVarTy Var
          | AppTy Type Type
          | TyConApp TyCon [Type]
          | FunTy Type Type
          | ForAllTy Var Type
          deriving (Show, Eq)

data Literal = LitInt  Integer  Type
             | LitChar Char     Type
             | LitFrac  Double Type
             | LitStr  String   Type
             deriving Show

data Expr = Var Var
          | Lit Literal
          | App Expr Expr
          | Lam [Var] Expr
          | Let Bind Expr
          | Case Expr Var [Alt]
          | Cast Expr Type
          | Type Type
          deriving Show

data Bind = NoRec Var Expr
          | Rec [(Var, Expr)]
          deriving Show

type Alt = (AltCon, [Var], Expr)

data AltCon = DataAlt DataCon
            | LitAlt Literal
            | DEFAULT
            deriving Show

data DataCon = DataCon Id [Var] Type
             deriving Show

ppModule :: Module -> Doc
ppModule (Module modident bs) =
  text "Module" <+> text modident <> line <$> ppBinds bs

ppBinds :: [Bind] -> Doc
ppBinds bs = list $ map ppBind bs

ppBind :: Bind -> Doc
ppBind b = text (show b)
