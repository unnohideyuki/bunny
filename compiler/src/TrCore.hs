module TrCore where -- Translate from Pattern.Expr to Core.

import           Core
import qualified Pattern                    as Pat
import           Symbol
import           Types
import           Typing                     (Assump (..), Pred (..), Qual (..),
                                             Scheme (..), find, inst)
import qualified Typing                     as Ty (Expr (..), Literal (..),
                                                   Pat (..))

import           Control.Monad.State.Strict
import           Debug.Trace

ptypes :: Type -> [Type]
ptypes t =
 let
   ptypes' ts (TAp
               (TAp
                (TCon (Tycon
                       "(->)" (Kfun Star (Kfun Star Star))))
                t1)
               t2)
     = ptypes' (ts ++ [t1]) t2
   ptypes' ts _ = ts
 in
  ptypes' [] t

-- Translation Monad

data TrcState = TrcState { trc_as     :: [Assump]
                         , trc_bind   :: Bind
                         , trc_bstack :: [Bind]
                         , trc_num    :: Int
                         }

type TRC a = State TrcState a

enumId' :: Int -> Id
enumId' n = ".t" ++ show n

newTVar' :: Kind -> TRC Type
newTVar' k = do st <- get
                let n = trc_num st
                put st{trc_num = n + 1}
                return $ TVar (Tyvar (enumId' n) k)

freshInst' :: Scheme -> TRC (Qual Type)
freshInst' (Forall ks qt) = do ts <- mapM newTVar' ks
                               return (inst ts qt)

translateVdefs :: [Assump] -> [(Id, Pat.Expression)] -> Bind
translateVdefs as vdefs =
  let
    st = TrcState as (Rec []) [] 0
    (_, st') = runState (transVdefs vdefs) st
  in
   trc_bind st'

typeLookup :: Id -> TRC (Qual Type)
typeLookup n = do
  as <- getAs
  let sc = case find n as of
        Just sc' -> sc'
        Nothing  -> error $ "type not found:" ++ n
  freshInst' sc

getAs :: TRC [Assump]
getAs = do
  st <- get
  return $ trc_as st

putAs :: [Assump] -> TRC ()
putAs as = do
  st <- get
  put st{trc_as = as}

appendAs :: [Assump] -> TRC ()
appendAs as' = do
  as <- getAs
  putAs (as' ++ as)

appendBind :: (Id, Expr) -> TRC ()
appendBind (n, e) = do
  st <- get
  t <- typeLookup n
  let bs = case trc_bind st of
        Rec bs' -> bs'
        _       -> error "must not reach."
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
  let sc = case find n as of Just sc' -> sc'
                             Nothing  -> error $ "type not found 1:" ++ show (n, as)
  qt <- freshInst' sc
  let ks = case sc of
        Forall ks' _ -> ks'
      ts = trace ("transLam:" ++ show qt) $ case qt of
        (_ :=> t') -> ptypes t'
      vs = map f $ zip ns ts
      qf = case qt of
        (qf' :=> _) -> qf'
      f (n', t') = TermVar n' (qf :=> t')
      as' = [n' :>: Forall [] (qf :=> t') | (n', t') <- zip ns ts]
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
    trClauses ((Pat.Clause a@(n :>: sc) ns expr):cs) alts = do
      qt <- freshInst' sc
      let
        ks = case sc of
          Forall ks' _ -> ks'
        ts = case qt of
          (_ :=> t') -> ptypes t'
        vs = map (\(n', t') -> TermVar n' ([] :=> t')) $ zip ns ts
        as' = [n' :>: (Forall ks ([] :=> t')) | (n', t') <- zip ns ts]
      as <- getAs
      appendAs as'
      expr' <- transExpr expr
      putAs as
      let alt = (DataAlt (DataCon n vs qt), [], expr')
      trClauses cs (alt:alts)

    {- for temporary fix (#t001) -}
    trClauses ((Pat.DefaultClause n expr):cs) alts = do
      expr' <- transExpr expr
      let v = TermVar n undefined -- with dummy type
          alt = (DEFAULT, [v], expr')
      trClauses cs (alt:alts)

{- Todo: fix this.
   This is a temprary fix (#t001) to support default alts.
   See the note page 212.
-}
transExpr (Pat.Fatbar e (Pat.Case n cs)) = transExpr $ Pat.Case n cs'
  where cs' = addDefAlt cs []
        defcls = Pat.DefaultClause n e
        addDefAlt [] ncs = reverse $ (defcls:ncs)
        addDefAlt (cls@(Pat.Clause _ _ expr):cs) ncs = case expr of
          Pat.Error -> addDefAlt cs ncs
          _         -> addDefAlt cs (cls:ncs)

transExpr Pat.Error = return $ Var (TermVar "Prim.neErr" undefined)

transExpr e = error $ "Non-exaustive Patterns in transExpr: " ++ show e


trExpr2 :: Ty.Expr -> TRC Expr
trExpr2 (Ty.Var n) = do
  t <- typeLookup n
  return $ Var (TermVar n t)

trExpr2 (Ty.Lit (Ty.LitStr s)) = return e
  where
    e = Lit $ LitStr s tString

trExpr2 (Ty.Lit (Ty.LitChar c)) = return e
  where
    e = Lit $ LitChar c tChar

trExpr2 (Ty.Lit (Ty.LitInt n)) = return e
  where
    e = Lit $ LitInt n tInteger

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

trExpr2 (Ty.Const (n :>: sc)) = do
  qt <- freshInst' sc
  return $ Var $ TermVar n qt

trExpr2 expr = error $ "Non-exaustive patterns in trExpr2: " ++ show expr

dsgIs vds [] = vds
dsgIs vds (impl:is) = dsgIs (desis impl:vds) is
  where
    desis (n, alts) = (n, dsgAlts n $ cnvalts alts)

dsgAlts n alts@((pats,_):_) =
  let
    k = length pats
    us = [Pat.mkVar n i| i <- [1..k]]

    alts' = rmWild alts [] -- for temporary fix (#t002), see the note p.212
    rmWild [] as                         = reverse as
    rmWild (([Ty.PWildcard], e):alts) as = rmWild alts (([Ty.PVar "_"], e):as)
    rmWild (alt:alts) as                 = rmWild alts (alt:as)

    e = Pat.match n k us alts' Pat.Error
  in
   Pat.Lambda us e

cnvalts alts =
  fmap (\(pats, e) -> (pats, Pat.OtherExpression e)) alts
