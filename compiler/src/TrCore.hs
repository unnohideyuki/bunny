module TrCore where -- Translate from Pattern.Expr to Core.

import Symbol
import Core
import qualified Pattern as Pat
import qualified Typing as Ty
import qualified Types as Ty

import Control.Monad.State.Strict
import Debug.Trace

trKind :: Ty.Kind -> Kind
trKind Ty.Star = Star
trKind (Ty.Kfun k1 k2) = Kfun (trKind k1) (trKind k2)

isFunTyCon :: TyCon -> Bool
isFunTyCon (TyCon "(->)" (Kfun Star (Kfun Star Star))) = True
isFunTyCon _                                           = False

trType :: Ty.Type -> [Var] -> Type
trType (Ty.TCon (Ty.Tycon n k)) _ = TyConApp (TyCon n (trKind k)) []
trType (Ty.TAp t1 t2) vs =
  let
    t1' = trType t1 vs
    t2' = trType t2 vs
  in
   case t1' of
     TyConApp tycon ts | isFunTyCon tycon && length ts == 1
                         -> FunTy (head ts) t2'
                       | otherwise
                         -> TyConApp tycon (ts ++ [t2'])
     _                 -> AppTy t1' t2'
     
trType (Ty.TGen n) vs =
  if length vs > n
  then TyVarTy $ vs !! n 
  else error $ "(!!): index too large: " ++ show (vs, n)

trType (Ty.TVar (Ty.Tyvar n k)) _ = TyVarTy $ TypeVar n $ trKind k

trType t _ = error $ "Non-exhaustive patterns in trType: " ++ show t

ptypes :: Ty.Type -> [Ty.Type]
ptypes t =
 let
   ptypes' ts (Ty.TAp
               (Ty.TAp
                (Ty.TCon (Ty.Tycon
                          "(->)" (Ty.Kfun Ty.Star (Ty.Kfun Ty.Star Ty.Star))))
                t1)
               t2)
     = ptypes' (ts ++ [t1]) t2
   ptypes' ts _ = ts
 in
  ptypes' [] t

-- Translation Monad

data TrcState = TrcState { trc_as :: [Ty.Assump]
                         , trc_bind :: Bind
                         , trc_bstack :: [Bind]
                         }

type TRC a = State TrcState a

translateVdefs :: [Ty.Assump] -> [(Id, Pat.Expression)] -> Bind
translateVdefs as vdefs = 
  let
    st = TrcState as (Rec []) []
    (_, st') = runState (transVdefs vdefs) st
  in
   trc_bind st'

typeLookup :: Id -> TRC Type
typeLookup n = do
  as <- getAs
  let sc = case Ty.find n as of
        Just sc' -> sc'
        Nothing  -> error $ "type not found:" ++ n
      t = case sc of
        Ty.Forall ks (_ Ty.:=> t) -> trType t (ks2TVars ks)
  return t

ks2TVars ks = ks2tvars $ zip ks [0,1..]
  where
    ks2tvars [] = []
    ks2tvars ((k,n):xs) =
      TypeVar ("a" ++ show n) (trKind k) : ks2tvars xs
  

getAs :: TRC [Ty.Assump]
getAs = do
  st <- get
  return $ trc_as st

putAs :: [Ty.Assump] -> TRC ()
putAs as = do
  st <- get
  put st{trc_as = as}

appendAs :: [Ty.Assump] -> TRC ()
appendAs as' = do
  as <- getAs
  putAs (as' ++ as)

appendBind :: (Id, Expr) -> TRC ()
appendBind (n, e) = do
  st <- get
  t <- typeLookup n
  let bs = case trc_bind st of
        Rec bs' -> bs'
        _ -> error "must not reach."
      bs' = bs ++ [(TermVar n t, e)]
  put st{trc_bind = Rec bs'}

pushBind :: TRC ()
pushBind = do
  st <- get
  let bs = trc_bstack st
      b = trc_bind st
      st' = st{trc_bind = Rec [], trc_bstack = (b:bs)}
  put st'

popBind :: TRC Bind
popBind = do
  st <- get
  let (b:bs) = trc_bstack st
      st' = st{trc_bind=b, trc_bstack=bs}
  put st'
  return $ trc_bind st
      
transVdef :: (Id, Pat.Expression) -> TRC ()
transVdef (n, Pat.Lambda ns expr) = do
  as <- getAs
  let sc = case Ty.find n as of Just sc' -> sc'
                                Nothing  -> error $ "type not found 1:" ++ show (n, as)
      (ts, ks) = case sc of
        Ty.Forall ks' (_ Ty.:=> t') -> (ptypes t', ks')
      vs = map (\(n', t') -> TermVar n' (trType t' (ks2TVars ks))) $ zip ns ts
      as' = [n' Ty.:>: (Ty.Forall ks ([] Ty.:=> t')) | (n', t') <- zip ns ts]
  appendAs as'
  expr' <- transExpr expr
  appendBind (n, lam' vs expr')
  putAs as
  where
    lam' [] e = e
    lam' vs e = Lam vs e

transVdef (n, e) = do
  expr' <- transExpr e
  appendBind (n, expr')

transVdefs :: [(Id, Pat.Expression)] -> TRC ()
transVdefs [] = return ()
transVdefs (vd:vds) = do
  transVdef vd
  transVdefs vds

transExpr :: Pat.Expression -> TRC Expr
transExpr (Pat.Fatbar e Pat.Error) = transExpr e

transExpr (Pat.OtherExpression e) =
  do
    as <- getAs
    trExpr2 e

transExpr (Pat.Case n cs) = do
  vt <- typeLookup n
  let scrut = Var (TermVar n vt)
      case_bndr = TermVar (n++"b") vt -- todo: it's just a dummy!
  alts <- trClauses cs []
  return $ Case scrut case_bndr alts
  where 
    trClauses [] alts = return alts
    trClauses ((Pat.Clause a@(n Ty.:>: sc) ns expr):cs) alts = do
      let
        (ts, ks) = case sc of
          Ty.Forall ks' (_ Ty.:=> t') -> (ptypes t', ks')
        vs = map (\(n', t') -> TermVar n' (trType t' (ks2TVars ks))) $ zip ns ts
        as' = [n' Ty.:>: (Ty.Forall ks ([] Ty.:=> t')) | (n', t') <- zip ns ts]
      as <- getAs
      appendAs as'
      expr' <- transExpr expr
      putAs as
      let t = case sc of
            Ty.Forall ks (_ Ty.:=> t') -> trType t' (ks2TVars ks)
          alt = (DataAlt (DataCon n [] t), [], expr')
      trClauses cs (alt:alts)

transExpr e = error $ "Non-exaustive Patterns in transExpr: " ++ show e


trExpr2 :: Ty.Expr -> TRC Expr
trExpr2 (Ty.Var n) = do
  t <- typeLookup n
  return $ Var (TermVar n t)

trExpr2 (Ty.Lit (Ty.LitStr s)) = return e
  where
    t = (TyConApp
         (TyCon "[]" (Kfun Star Star)) [TyConApp (TyCon "Char" Star) []])
    e = Lit $ LitStr s t

trExpr2 (Ty.Lit (Ty.LitChar c)) = return e
  where
    t = TyConApp (TyCon "Char" Star) []
    e = Lit $ LitChar c t

trExpr2 (Ty.Lit (Ty.LitInt n)) = return e
  where
    t = TyConApp (TyCon "Integer" Star) [] -- Shoud be?: Num a => a
    e = Lit $ LitInt n t

trExpr2 (Ty.Ap e1 e2) = do
  e1' <- trExpr2 e1
  e2' <- trExpr2 e2
  return $ App e1' e2'

trExpr2 (Ty.Let bg e) = do
  pushBind
  transVdefs vdefs
  b' <- popBind
  e' <- trExpr2 e
  return $ Let b' e'
  where
    (es, iss) = bg
    is = concat iss
    vdefs = dsgIs [] is

trExpr2 (Ty.Const (n Ty.:>: sc)) = do
  return $ Var $ TermVar n t
  where
    t = case sc of
      Ty.Forall ks (_ Ty.:=> t') -> trType t' (ks2TVars ks)

trExpr2 expr = error $ "Non-exaustive patterns in trExpr2: " ++ show expr


dsgIs vds [] = vds
dsgIs vds (impl:is) = dsgIs (desis impl:vds) is
  where
    desis (n, alts) = (n, dsgAlts $ cnvalts alts)

dsgAlts alts@((pats,_):_) =
  let
    k = length pats
    us = [Pat.mkVar i | i <- [1..k]]
    e = Pat.match k us alts Pat.Error
  in
   Pat.Lambda us e

cnvalts alts =
  fmap (\(pats, e) -> (pats, Pat.OtherExpression e)) alts



