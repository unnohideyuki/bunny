module TrCore where -- Translate from Pattern.Expr to Core.

import Symbol
import Core
import qualified Pattern as Pat
import qualified Typing as Ty
import qualified Types as Ty

import Debug.Trace

trKind :: Ty.Kind -> Kind
trKind Ty.Star = Star
trKind (Ty.Kfun k1 k2) = Kfun (trKind k1) (trKind k2)

isFunTyCon :: TyCon -> Bool
isFunTyCon (TyCon "(->)" (Kfun Star (Kfun Star Star))) = True
isFunTyCon _                                           = False

trType :: Ty.Type -> Type
trType (Ty.TCon (Ty.Tycon n k)) = TyConApp (TyCon n (trKind k)) []
trType (Ty.TAp t1 t2) =
  let
    t1' = trType t1
    t2' = trType t2
  in
   case t1' of
     TyConApp tycon ts | isFunTyCon tycon && length ts == 1 -> FunTy (head ts) t2'
                       | otherwise                          -> TyConApp tycon (ts ++ [t2'])
     _                 -> AppTy t1' t2'

-- fix me : the order of ID and [Assump]
tyLookup :: Id -> [Ty.Assump] -> Type
tyLookup n as = let
  sc = case Ty.find n as of
    Just sc -> sc
    Nothing -> error $ "type not found: " ++ n
  in
   case sc of
     Ty.Forall [] ([] Ty.:=> t) -> trType t
     _                       -> error $ "forall not supported yet: " ++ show sc

trExpr :: [Ty.Assump] -> (Id, Pat.Expression) -> Expr
trExpr as (n, (Pat.Fatbar e Pat.Error)) = trExpr as (n, e)

trExpr as (n, (Pat.OtherExpression e)) =
  let
    trexpr (Ty.Ap e1 e2) = App (trexpr e1) (trexpr e2)
    trexpr (Ty.Var n) = Var (TermVar n (tyLookup n as))
    trexpr (Ty.Lit (Ty.LitStr s)) = Lit $
      LitStr
      s
      (TyConApp (TyCon "[]" (Kfun Star Star)) [TyConApp (TyCon "Char" Star) []])
  in
   trexpr e

trExpr as (n, (Pat.Lambda ns expr)) =
  let
    -- fixe me : use tyLookup here rather than Ty.find
    sc = case Ty.find n as of
      Just sc -> sc
      Nothing -> error $ "type not found: " ++ n
    ts = case sc of
      Ty.Forall [] ([] Ty.:=> t') -> ptypes t'
      _ -> error $ "forall not supported yet (2): " ++ show sc
    vs = map (\(n, t) -> TermVar n (trType t)) $ zip ns ts

    lam' (v:vs) e = Lam v $ lam' vs e
    lam' [] e = e

    expr' = Var (TypeVar "dummy" Star)
    {-
    It can not as "expr' = trExpr as ("", expr)" because
    ns and ts information is needed to translate the expr here.
    -- expr' = trpat expr
    -}
  in
   lam' vs expr'


trExpr as e = error $ "Non-exaustive patterns in trExpr, " ++ show e

ptypes :: Ty.Type -> [Ty.Type]
ptypes t =
 let
   ptypes' ts (Ty.TAp
               (Ty.TAp
                (Ty.TCon (Ty.Tycon "(->)" (Ty.Kfun Ty.Star (Ty.Kfun Ty.Star Ty.Star))))
                t1)
               t2)
     = ptypes' (ts ++ [t1]) t2
   ptypes' ts _ = ts
 in
  ptypes' [] t

