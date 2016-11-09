module Core where

import Symbol
import Types
import Typing (Qual(..))

data Module = Module Id [Bind] {- [Axiom] -}

data Var = TermVar Id (Qual Type)
         {- | TypeVar Id Kind -} -- unused
         deriving (Show, Eq)

data Literal = LitInt  Integer  Type
             | LitChar Char     Type
             | LitFrac  Double Type
             | LitStr  String   Type
             deriving (Show, Eq)

data Dict = Dict Id
          deriving (Show, Eq)

data Expr = Var Var
          | Lit Literal
          | App Expr Expr
          | Lam [Var] Expr
          | Let Bind Expr
          | Case Expr Var [Alt]
          | Dps Var Dict -- Dictionary Passing Style
          {- | Cast Expr Type -} -- unused
          {- | Type Type -} -- unused
          deriving (Show, Eq)

data Bind = {- NoRec Var Expr
          | -} Rec [(Var, Expr)]
          deriving (Show, Eq)

type Alt = (AltCon, [Var], Expr)

data AltCon = DataAlt DataCon
            | LitAlt Literal
            | DEFAULT
            deriving (Show, Eq)

data DataCon = DataCon Id [Var] (Qual Type)
             deriving (Show, Eq)
