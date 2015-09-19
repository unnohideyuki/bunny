module Pattern where

import Data.List hiding (partition)
import Data.List.Split

import qualified Typing
import qualified PreDefined
import Symbol

arity :: Typing.Assump -> Int
arity (c Typing.:>: _) =
  case c of
    "Prim.()" -> 0
    "Prim.[]" -> 0
    "Prim.:"  -> 2
    "Prim.True"  -> 0
    "Prim.False" -> 0
    _ -> error $ "unknown arity: " ++ c

constructors :: Typing.Assump -> [Typing.Assump]
constructors (c Typing.:>: _) =
  case c of
    "Prim.()" -> [PreDefined.unitCfun]
    "Prim.[]" -> [PreDefined.nilCfun, PreDefined.consCfun]
    "Prim.:"  -> [PreDefined.nilCfun, PreDefined.consCfun]
    "Prim.True"  -> [PreDefined.falseCfun, PreDefined.trueCfun]
    "Prim.False" -> [PreDefined.falseCfun, PreDefined.trueCfun]
    _ -> error $ "unknown constructors: " ++ c

data Expression = Case Variable [Clause]
                | Fatbar Expression Expression
                | OtherExpression Typing.Expr
                deriving Show

type Variable = Id

data Clause = Clause Typing.Assump [Variable] Expression
            deriving Show

subst :: Expression -> Variable -> Variable -> Expression
subst expr vnew vold =
  let
    subst_var v | v == vold = vnew
                | otherwise = v

    subst_vs vs = fmap (\v -> subst_var v) vs

    subst_c (Clause c vs e) = Clause c (subst_vs vs) (subst e vnew vold)

    subst_cs cs = fmap (\c -> subst_c c) cs

    subst_expr e = Typing.subst e vnew vold
  in
   case expr of
     Case v cs -> Case (subst_var v) (subst_cs cs)
     Fatbar e1 e2 -> Fatbar (subst e1 vnew vold) (subst e2 vnew vold)
     OtherExpression e -> OtherExpression (subst_expr e)

