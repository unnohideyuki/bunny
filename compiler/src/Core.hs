module Core where

import           Symbol
import           Types
import           Typing (Qual (..))

data Module = Module Id [Bind] {- [Axiom] -}

data Var = TermVar Id (Qual Type)
         | DictVar Id Id -- Should it be DictCon?
         {- | TypeVar Id Kind -} -- unused
         deriving (Show, Eq)

data Literal = LitInt  Integer  Type
             | LitChar Char     Type
             | LitFrac  Double Type
             | LitStr  String   Type
             deriving (Show, Eq)

newtype Dict = Dict Id deriving (Show, Eq)

data Expr = Var Var
          | Lit Literal
          | App Expr Expr
          | Lam [Var] Expr
          | Let Bind Expr
          | Case Expr Var [Alt]
          {- | Cast Expr Type -} -- unused
          {- | Type Type -} -- unused
          deriving (Show, Eq)

newtype Bind = {- NoRec Var Expr
             | -} Rec [(Var, Expr)]
             deriving (Show, Eq)

type Alt = (AltCon, [Var], Expr)

data AltCon = DataAlt DataCon
            | LitAlt Literal
            | DEFAULT
            deriving (Show, Eq)

data DataCon = DataCon Id [Var] (Qual Type)
             deriving (Show, Eq)

{-
-- Pretty Print

-- Var

ppVar :: Var -> String
ppVar (TermVar n qt) = "(" ++ n ++ "::" ++ pp qt ++ ")"
ppVar (DictVar x y) = "${" ++ x ++ " " ++ y ++ "}"

-- Literal

ppLit :: Literal -> String

ppLit (LitInt n t) = "(" ++ show n ++ "::" ++ show t ++ ")"

ppLit (LitChar c _) = show c

ppLit (LitStr s _) = show s

ppLit l = show l

-- Expr
ppExpr :: Expr -> String

ppExpr (Var v) = pp v

ppExpr (Lit l) = pp l

ppExpr (App e1 e2) = "(" ++ pp e1 ++ " " ++ pp e2 ++ ")"

ppExpr (Lam vs e) = "(\\" ++ pp vs ++ " -> " ++ pp e ++ ")"

ppExpr (Let b e) = "(let " ++ pp b ++ " in " ++ pp e ++ ")"

ppExpr (Case e v as) = "(case " ++ pp e ++ " " ++ pp v ++ " " ++
                       ppAlts as ++ ")"

ppAlt :: Alt -> String
ppAlt (ac, vs, e) = "(" ++ pp ac ++ "," ++ pp vs ++ "," ++ pp e ++ ")"

ppAlts :: [Alt] -> String
ppAlts as = "[" ++ intercalate ", " (map ppAlt as) ++ "]"

ppAltCon :: AltCon -> String
ppAltCon (DataAlt (DataCon n vs qt)) = n ++ " " ++ s ++ " :: " ++ pp qt
  where s = intercalate " " (map pp vs)

ppAltCon x = show x

ppBind :: Bind -> String
ppBind (Rec bs) = "[" ++ intercalate ", " (map ppbind bs) ++ "]"
  where ppbind (v, e) = "(" ++ pp v ++ "," ++ pp e ++ ")"

-}
