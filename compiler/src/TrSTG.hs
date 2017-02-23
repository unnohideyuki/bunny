module TrSTG where

import qualified Core
import qualified Typing as Ty
import STG

import Debug.Trace

trVar (Core.TermVar n _) = TermVar n

trVar (Core.DictVar n1 n2) = DictVar n1 n2

trLit (Core.LitInt i _) = LitInt i

trLit (Core.LitChar c _) = LitChar c

trLit (Core.LitStr s _) = LitStr s

trLit (Core.LitFrac r _) = LitFrac r

trExpr (Core.Var v) = AtomExpr $ VarAtom $ trVar v

trExpr (Core.Lit l) = AtomExpr $ LitAtom $ trLit l

trExpr (Core.App f x) = FunAppExpr f' [x']
  where
    f' = trExpr f
    x' = trExpr x

trExpr (Core.Let b e) = 
  let
    bs = trBind b
    expr = trExpr e
  in
   LetExpr bs expr

trExpr e@(Core.Lam vs expr) = LamExpr vs' expr'
  where
    vs' = map trVar vs
    expr' = trExpr expr

trExpr e@(Core.Case scrut _ alts) = CaseExpr scrut' alts'
  where
    scrut' = trExpr scrut
    alts' = map tralt alts
    tralt ((Core.DataAlt (Core.DataCon name vs _)), _, expr) =
      CotrAlt name $ trExpr expr'
      where 
        expr' = case vs of
          [] -> expr
          vs' -> (Core.Lam vs' expr)
    tralt (Core.DEFAULT, vs, expr) = DefaultAlt $ trExpr (Core.Lam vs expr)

trExpr e = error $ "Non-exhaustive pattern in trExpr: " ++ show e

trbind :: (Core.Var, Core.Expr) -> Bind
trbind (v, e) = Bind (trVar v) (trExpr e)

trBind :: Core.Bind -> [Bind]
{- trBind (Core.NoRec v e) = [trbind (v, e)] -}
trBind (Core.Rec bs) = trbinds bs
  where
    trbinds [] = []
    trbinds (b:bs) = trbind b : trbinds bs

