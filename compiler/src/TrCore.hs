module TrCore where -- Translate from Pattern.Expr to Core.

import           Core
import qualified Pattern                    as Pat
import           PreDefined                 (initialConsts)
import           Symbol
import           Types
import           Typing                     (Assump (..), Qual (..),
                                             Scheme (..), find, inst)
import qualified Typing                     as Ty (Expr (..), Literal (..),
                                                   Pat (..))

import           Control.Monad.State.Strict
-- import           Debug.Trace

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

data TrcState = TrcState { trcAs     :: [Assump]
                         , trcBind   :: Bind
                         , trcBstack :: [Bind]
                         , trcNum    :: Int
                         }

type TRC a = State TrcState a

enumId' :: Int -> Id
enumId' n = "t" ++ show n

newTVar' :: Kind -> TRC Type
newTVar' k = do st <- get
                let n = trcNum st
                put st{trcNum = n + 1}
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
   trcBind st'

typeLookup :: Id -> TRC (Qual Type)
typeLookup n = do
  as <- getAs
  sc <- find n as
  freshInst' sc

getAs :: TRC [Assump]
getAs = do
  st <- get
  return $ trcAs st

putAs :: [Assump] -> TRC ()
putAs as = do
  st <- get
  put st{trcAs = as}

appendAs :: [Assump] -> TRC ()
appendAs as' = do
  as <- getAs
  putAs (as' ++ as)

appendBind :: (Id, Expr) -> TRC ()
appendBind (n, e) = do
  st <- get
  t <- typeLookup n
  let Rec bs = trcBind st
      bs' = bs ++ [(TermVar n t, e)]
  put st{trcBind = Rec bs'}

pushBind :: TRC ()
pushBind = do
  st <- get
  let bs = trcBstack st
      b = trcBind st
      st' = st{trcBind = Rec [], trcBstack = b:bs}
  put st'

popBind :: TRC Bind
popBind = do
  st <- get
  let (b:bs) = trcBstack st
      st' = st{trcBind=b, trcBstack=bs}
  put st'
  return $ trcBind st

transVdef :: (Id, Pat.Expression) -> TRC ()
transVdef (n, Pat.Lambda ns expr) = do
  as <- getAs
  sc <- find n as
  qt <- freshInst' sc
  let ts = case qt of
        (_ :=> t') -> ptypes t'
      vs = zipWith f ns ts
      qf = case qt of
        (qf' :=> _) -> qf'
      f n' t' = TermVar n' (qf :=> t')
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
transExpr (Pat.OtherExpression e) = trExpr2 e

-- TODO: I should have naming rules about cs, cs', n, i etc.
transExpr (Pat.Case n cs) = do
  vt <- typeLookup n
  let scrut = Var (TermVar n vt)
      case_bndr = TermVar (n++"b") vt -- todo: it's just a dummy!
  alts <- trClauses cs []
  return $ Case scrut case_bndr alts
  where
    trClauses [] alts = return alts
    trClauses (Pat.Clause (i :>: scm) ns expr : cs') alts = do
      qt <- freshInst' scm
      let
        ks = case scm of
          Forall ks' _ -> ks'
        ts = case qt of
          (_ :=> t') -> ptypes t'
        vs = map (\(n', t') -> TermVar n' ([] :=> t')) $ zip ns ts
        as' = [n' :>: Forall ks ([] :=> t') | (n', t') <- zip ns ts]
      as <- getAs
      appendAs as'
      expr' <- transExpr expr
      putAs as
      -- TODO: qt is the type of the constructor. Should it be the type of lhs?
      let alt = (DataAlt (DataCon i vs qt), [], expr')
      trClauses cs' (alt:alts)

    {- for temporary fix (#t001) -}
    trClauses (Pat.DefaultClause i expr : cs') alts = do
      expr' <- transExpr expr
      a <- newTVar' Star
      let v = TermVar i ([] :=> a)
          alt = (DEFAULT, [v], expr')
      trClauses cs' (alt:alts)

{- TODO: fix this.
   This is a temprary fix (#t001) to support default alts.
   See the note page 212.
-}
transExpr (Pat.Fatbar e (Pat.Case n cs)) = transExpr $ Pat.Case n cs'
  where cs' = addDefAlt cs []
        defcls = Pat.DefaultClause n e
        addDefAlt [] ncs = reverse (defcls:ncs)
        addDefAlt (cl@(Pat.Clause _ _ expr):cls) ncs = case expr of
          Pat.Error -> addDefAlt cls ncs
          _         -> addDefAlt cls (cl:ncs)
        addDefAlt _ _ = error "addDefAlt: not expected to reach here."

transExpr Pat.Error = do a <- newTVar' Star
                         return $ Var (TermVar "Prim.neErr" ([] :=> a))

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
    (_, iss) = bg
    is = concat iss
    vdefs = dsgIs [] is

trExpr2 (Ty.Const (n :>: sc)) = do
  qt <- freshInst' sc
  return $ Var $ TermVar n qt

trExpr2 expr = error $ "Non-exaustive patterns in trExpr2: " ++ show expr

dsgIs :: [(Id, Pat.Expression)]
      -> [(Id, [([Ty.Pat], Ty.Expr)])] -> [(Id, Pat.Expression)]

dsgIs vds [] = vds
dsgIs vds (impl:is) = dsgIs (desis impl:vds) is
  where
    desis (n, alts) = (n, dsgAlts n $ cnvalts alts)

dsgAlts :: Id -> [([Ty.Pat], Pat.Expression)] -> Pat.Expression

dsgAlts n alts@((pats,_):_) =
  let
    k = length pats
    us = [Pat.mkVar n i| i <- [1..k]]

    alts' = rmWild alts [] -- for temporary fix (#t002), see the note p.212
    rmWild [] as = reverse as
    rmWild (([Ty.PWildcard], e'):als') as =
      rmWild als' (([Ty.PVar "_"], e'):as)
    rmWild (alt:als') as = rmWild als' (alt:as)

    ci = initialConsts
    e = Pat.reduceMatch ci n k us alts' Pat.Error
  in
   Pat.Lambda us e

dsgAlts _ [] = error "dsgAlts: must not occur"

cnvalts :: [([Ty.Pat], Ty.Expr)] -> [([Ty.Pat], Pat.Expression)]
cnvalts = fmap (\(pats, e) -> (pats, Pat.OtherExpression e))
