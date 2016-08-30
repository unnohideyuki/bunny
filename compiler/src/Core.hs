module Core where

import Symbol
import Types
import Typing (Qual(..), Pred(..))

import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Module = Module Id [Bind] {- [Axiom] -}

data Var = TermVar Id (Qual Type)
         | TypeVar Id Kind
         deriving (Show, Eq)

{- 2016-08-30: Changed to use Types 
data Kind = Star | Kfun Kind Kind
          deriving (Show, Eq)

data TyCon = TyCon Id Kind
           deriving (Show, Eq)

data Type = TyVarTy Var
          | AppTy Type Type
          | TyConApp TyCon [Type]
          | FunTy Type Type
          | ForAllTy Var Type -- todo: not used?
          | DictTy Id Id -- 2 Ids: Dict-name, tyvar-name
          deriving (Show, Eq)
-}

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

data DataCon = DataCon Id [Var] (Qual Type)
             deriving Show

ppModule :: Module -> PP.Doc
ppModule (Module modident bs) =
  PP.text "Module" PP.<+> PP.text modident PP.<> PP.line PP.<$> ppBinds bs

ppBinds :: [Bind] -> PP.Doc
ppBinds bs = PP.list $ map ppBind bs

ppBind :: Bind -> PP.Doc
ppBind b = PP.text (show b)
