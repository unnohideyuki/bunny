module Core where

import Symbol
import Text.PrettyPrint.ANSI.Leijen

data Module = Module Id [Bind] {- [Axiom] -}

data Var = TermVar Id Type
         | TypeVar Id Kind

data Kind = Star | Kfun Kind Kind

data TyCon = TyCon Id Kind

data Type = TyVarTy Var
          | AppTy Type Type
          | TyConApp TyCon [Type]
          | FunTy Type Type
          | ForAllTy Var Type

data Literal = LitInt  Integer  Type
             | LitChar Char     Type
             | LitRat  Rational Type
             | LitStr  String   Type

data Expr = Var Var
          | Lit Literal
          | App Expr Expr
          | Lam Var Expr
          | Let Bind Expr
          | Case Expr Var [Alt]
          | Cast Expr Type
          | Type Type

data Bind = NoRec Var Expr
          | Rec [(Var, Expr)]

type Alt = (AltCon, [Var], Expr)

data AltCon = DataAlt DataCon
            | LitAlt Literal
            | DEFAULT

data DataCon = DataCon Id [Var] Type

ppModule :: Module -> Doc
ppModule (Module modident _) =
  text "Module" <+> text modident <> line