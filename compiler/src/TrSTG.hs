module TrSTG where

import qualified Core
import STG

trVar (Core.TermVar n _) = TermVar n

trLit (Core.LitStr s _) = LitStr s

trLit (Core.LitChar c _) = LitChar c

trExpr (Core.Var v) = AtomExpr $ VarAtom $ trVar v

trExpr (Core.Lit l) = AtomExpr $ LitAtom $ trLit l

trExpr (Core.App f x) = FunAppExpr f' [x']
  where
    f' = trExpr f
    x' = trExpr x

trExpr e = error $ "Non-exhaustive pattern in trExpr: " ++ show e

trbind :: (Core.Var, Core.Expr) -> Bind
trbind (v, e) = Bind (trVar v) (trExpr e)

trBind :: Core.Bind -> Program
trBind (Core.NoRec v e) = [trbind (v, e)]
trBind (Core.Rec bs) = trbinds bs
  where
    trbinds [] = []
    trbinds (b:bs) = trbind b : trbinds bs

