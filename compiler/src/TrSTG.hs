module TrSTG where

import qualified Core
import           STG

-- import           Debug.Trace

trVar :: Core.Var -> Var
trVar (Core.TermVar n _)   = TermVar n
trVar (Core.DictVar n1 n2) = DictVar n1 n2

trLit :: Core.Literal -> Literal
trLit (Core.LitInt i _)  = LitInt i
trLit (Core.LitChar c _) = LitChar c
trLit (Core.LitStr s _)  = LitStr s
trLit (Core.LitFrac r _) = LitFrac r

trExpr :: Core.Expr -> Expr
trExpr (Core.Var v) = AtomExpr $ VarAtom $ trVar v
trExpr (Core.Lit l) = AtomExpr $ LitAtom $ trLit l
trExpr (Core.App f x) = FunAppExpr (trExpr f) [trExpr x]
trExpr (Core.Let b e) = LetExpr (trBind b) (trExpr e)
trExpr (Core.Lam vs expr) = LamExpr (map trVar vs) (trExpr expr)

trExpr (Core.Case scrut _ alts) = CaseExpr (trExpr scrut) (map tralt alts)
  where
    tralt (Core.DataAlt (Core.DataCon name vs _), _, expr) =
      CotrAlt name $ trExpr expr'
      where
        expr' = case vs of
          []  -> expr
          vs' -> Core.Lam vs' expr
    tralt (Core.DEFAULT, vs, expr) = DefaultAlt $ trExpr (Core.Lam vs expr)
    tralt _ = error "tralt: must not occur"

trBind :: Core.Bind -> [Bind]
trBind (Core.Rec bs) = map trbind bs
  where trbind (v, e) = Bind (trVar v) (trExpr e)
