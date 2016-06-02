module STG where

import Prelude hiding((<$>))
import Text.PrettyPrint.ANSI.Leijen

import Symbol
import qualified Core

data Var = TermVar Id
         deriving Show

data Literal = LitStr String
             deriving Show

data Atom = VarAtom Var
          | LitAtom Literal
          deriving Show

data Expr = AtomExpr Atom
          | FunAppExpr Expr [Expr]
          deriving Show

data Bind = Bind Var Expr
          deriving Show

type Program = [Bind]
