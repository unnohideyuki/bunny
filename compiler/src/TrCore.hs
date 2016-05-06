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

trType :: Ty.Type -> Type
trType (Ty.TCon (Ty.Tycon n k)) = TyConApp (TyCon n (trKind k)) []
trType (Ty.TAp t1 t2) =
  let
    t1' = trType t1
    t2' = trType t2
  in
   case t1' of
     TyConApp tycon ts | isFunTyCon tycon && length ts == 1
                         -> FunTy (head ts) t2'
                       | otherwise
                         -> TyConApp tycon (ts ++ [t2'])
     _                 -> AppTy t1' t2'

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
                         , trc_binds :: [Bind]
                         }

type TRC a = State TrcState a

translateVdefs :: [Ty.Assump] -> [(Id, Pat.Expression)] -> [Bind]
translateVdefs as vdefs = 
  let
    st = TrcState as []
    (_, st') = runState (transVdefs vdefs) st
  in
   trc_binds st'

typeLookup :: Id -> TRC Type
typeLookup n = do
  st <- get
  let as = trc_as st
      sc = case Ty.find n as of
        Just sc' -> sc'
        Nothing  -> error $ "type not found:" ++ n
      t = case sc of
        Ty.Forall [] ([] Ty.:=> t) -> trType t
        _ -> error $ "forall not supported yet: " ++ show sc
  return t

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
  st <- get
  as <- getAs
  put st{trc_as = as ++ as'}

appendBind :: (Id, Expr) -> TRC ()
appendBind (n, e) = do
  st <- get
  t <- typeLookup n
  let b = NoRec (TermVar n t) e
      bs' = trc_binds st ++ [b]
  put st{trc_binds = bs'}

transVdef :: (Id, Pat.Expression) -> TRC ()
transVdef (n, Pat.Lambda ns expr) = do
  as <- getAs
  let sc = case Ty.find n as of Just sc' -> sc'
                                Nothing  -> error $ "type not found:" ++ show n
      ts = case sc of
        Ty.Forall [] ([] Ty.:=> t') -> ptypes t'
        _ -> error $ "forall types not supported yet (transVdef):" ++ show sc
      vs = map (\(n', t') -> TermVar n' (trType t')) $ zip ns ts
      as' = [n' Ty.:>: Ty.toScheme t' | (n', t') <- zip ns ts]
  appendAs as'
  expr' <- transExpr expr
  appendBind (n, lam' vs expr')
  where lam' (v:vs) e = Lam v $ lam' vs e
        lam' [] e = e

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
    return $ trexpr as e
  where
    trexpr as (Ty.Ap e1 e2) = App (trexpr as e1) (trexpr as e2)
    trexpr as (Ty.Var n) = Var (TermVar n (tyLookup n as))
    trexpr as (Ty.Lit (Ty.LitStr s)) = Lit $
      LitStr
      s
      (TyConApp (TyCon "[]" (Kfun Star Star)) [TyConApp (TyCon "Char" Star) []])

    trexpr _ expr = error $ "Non-exaustive patterns in trexpr: " ++ show expr

    -- fix me : the order of ID and [Assump]
    tyLookup :: Id -> [Ty.Assump] -> Type
    tyLookup n as = let
      sc = case Ty.find n as of
        Just sc -> sc
        Nothing -> error $ "type not found: " ++ n
      in
       case sc of
         Ty.Forall [] ([] Ty.:=> t) -> trType t
         _ -> error $ "forall not supported yet: " ++ show sc

transExpr (Pat.Case n cs) = do
  vt <- typeLookup n
  let scrut = Var (TermVar n vt)
      case_bndr = TermVar (n++"b") vt -- todo: it's just a dummy!
  alts <- trClauses cs []
  return $ Case scrut case_bndr alts
  where 
    trClauses [] alts = return alts
    trClauses ((Pat.Clause a@(n Ty.:>: sc) vs expr):cs) alts = do
      expr' <- transExpr expr
      let t = case sc of
            Ty.Forall [] ([] Ty.:=> t') -> trType t'
            _ -> error $ "forall not supported yet: " ++ show sc
          alt = (DataAlt (DataCon n [] t), [], expr')
      trClauses cs (alt:alts)

transExpr e = error $ "Non-exaustive Patterns in transExpr: " ++ show e


