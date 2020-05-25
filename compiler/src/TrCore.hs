module TrCore where -- Translate from Pattern.Expr to Core.

import           Core
import qualified Pattern                    as Pat
import           PreDefined                 (ConstructorInfo, initialConsts)
import           Symbol
import           Types
import           Typing                     (Assump (..), Assumps, Pred (..),
                                             Qual (..), Scheme (..), find, inst,
                                             tv)
import qualified Typing                     as Ty (Expr (..), Literal (..),
                                                   Pat (..))

import           Control.Monad.State.Strict
import           Data.List.Split            (splitOn)
import qualified Data.Map.Strict            as Map
import           Data.Maybe
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

data TrcState = TrcState { trcAs     :: Assumps
                         , trcBind   :: Bind
                         , trcBstack :: [Bind]
                         , trcNum    :: Int
                         , trcConsts :: ConstructorInfo
                         , trcFailSc :: Scheme
                         }

type TRC a = State TrcState a

putFailSc :: Scheme -> TRC ()
putFailSc sc = do st <- get
                  put st{trcFailSc = sc}

getCi :: TRC ConstructorInfo
getCi = do
  st <- get
  return $ trcConsts st

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

translateVdefs ::
  Assumps -> [(Id, Pat.Expression)] -> ConstructorInfo -> Bind
translateVdefs as vdefs ci =
  let
    st = TrcState as (Rec []) [] 0 ci (Forall [] ([] :=> tUnit)) -- tUnit is dummy
    (_, st') = runState (transVdefs vdefs) st
  in
   trcBind st'

typeLookup :: Id -> TRC (Qual Type)
typeLookup n = do
  as <- getAs
  sc <- find n as
  freshInst' sc

getAs :: TRC Assumps
getAs = do
  st <- get
  return $ trcAs st

putAs :: Assumps -> TRC ()
putAs as = do
  st <- get
  put st{trcAs = as}

appendAs :: Assumps -> TRC ()
appendAs as' = do
  as <- getAs
  putAs (Map.union as' as)

appendBind :: (Id, Expr) -> TRC ()
appendBind (n, e) = do
  t <- typeLookup n
  scfail <- trcFailSc <$> get
  tfail <- freshInst' scfail
  let isfail = last (splitOn "." n) == "_fail#"
      t' = if isfail then tfail else t
  st <- get -- this must be after the typeLookup above.
  let Rec bs = trcBind st
      bs' = bs ++ [(TermVar n t', e)]
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

calcFailSc :: Scheme -> [a] -> Scheme
calcFailSc sc [] = sc -- todo: simplify ks and ps
calcFailSc (Forall ks
            (ps :=> TAp (TAp (TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))) _) t'))
           (a:as)
  = calcFailSc (Forall ks (ps :=> t')) as

transVdef :: (Id, Pat.Expression) -> TRC ()
transVdef (n, Pat.Lambda ns expr) = do
  as <- getAs
  sc <- find n as
  qt <- freshInst' sc
  let failsc = calcFailSc sc ns
  let ts = case qt of
        (_ :=> t') -> ptypes t'
      vs = zipWith f ns ts
      qf = case qt of
        (qf' :=> _) -> qf'
      f n' t' = let qf' = filter (\pr -> head (tv pr) `elem` tv t') qf
                in TermVar n' (qf' :=> t')
      as' = Map.fromList [(n', Forall [] (qf' :=> t')) | TermVar n' (qf' :=> t') <- vs]

  appendAs as'

  failsc_save <- trcFailSc <$> get
  putFailSc failsc
  expr' <- transExpr expr

  st <- get
  put st{trcFailSc=failsc_save}
  appendBind (n, lam' vs expr')
  putAs as
  where
    lam' [] e = e
    lam' vs e = Lam vs e

transVdef (n, e) = do
  as <- getAs
  sc <- find n as
  putFailSc sc
  expr' <- transExpr e
  appendBind (n, expr')

transVdefs :: [(Id, Pat.Expression)] -> TRC ()
transVdefs [] = return ()
transVdefs (vd:vds) = do
  transVdef vd
  transVdefs vds

transExpr :: Pat.Expression -> TRC Expr

transExpr (Pat.OtherExpression e) = trExpr2 e

transExpr (Pat.Case n cs) = do
  vt <- typeLookup n
  let scrut = Var (TermVar n vt)
      case_bndr = TermVar (n++"b") vt -- todo: it's just a dummy!
  alts <- trClauses cs []
  return $ Case scrut case_bndr alts
  where
    trClauses [] alts = do
      return alts
    trClauses (Pat.Clause (i :>: scm) ns expr : cs') alts = do
      qt <- freshInst' scm
      let
        ks = case scm of
          Forall ks' _ -> ks'
        ts = case qt of
          (_ :=> t') -> ptypes t'
        vs = map (\(n', t') -> TermVar n' ([] :=> t')) $ zip ns ts
        as' = Map.fromList [(n', Forall ks ([] :=> t')) | (n', t') <- zip ns ts]
      as <- getAs
      appendAs as'
      expr' <- transExpr expr
      putAs as
      -- TODO: qt is the type of the constructor. Should it be the type of lhs?
      let alt = (DataAlt (DataCon i vs qt), [], expr')
      trClauses cs' (alt:alts)

transExpr (Pat.Fatbar (Pat.OtherExpression e) f) = do
  f' <- transExpr f
  r <- trExpr2 e
  let r' = replaceFailExpr f' r
  return r'
    where replaceFailExpr :: Expr -> Expr -> Expr
          replaceFailExpr f (Var v) = replaceFailVar f v
          replaceFailExpr _ l@(Lit _) = l
          replaceFailExpr f (App e1 e2) = let e1' = replaceFailExpr f e1
                                              e2' = replaceFailExpr f e2
                                          in App e1' e2'
          replaceFailExpr f (Lam vs e) = Lam vs (replaceFailExpr f e)
          replaceFailExpr f (Let b e) = Let (replaceFailBind f b) (replaceFailExpr f e)
          replaceFailExpr f (Case s v as) = Case s v (map (replaceFailAlt f) as)


          replaceFailVar f (TermVar n _) | n == "Prim.FAIL" = f
          replaceFailVar _ v             = Var v

          replaceFailBind f (Rec bs) =
            let bs' = map (\(v, e) -> (v, replaceFailExpr f e)) bs
            in Rec bs'

          replaceFailAlt f (altcon, vs, e) = (altcon, vs, replaceFailExpr f e)


--transExpr Pat.Error = do a <- newTVar' Star
--                         return $ Var (TermVar "Prim.neErr" ([] :=> a))

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

trExpr2 (Ty.Lit (Ty.LitInt n)) = do
  v <- newTVar' Star
  let qty = [IsIn "Prelude.Num" v] :=> (tInteger `fn` v)
      f = Var (TermVar "Prelude.fromInteger" qty)
      i = Lit (LitInt n tInteger)
  return (App f i)

trExpr2 (Ty.Ap e1 e2) = do
  e1' <- trExpr2 e1
  e2' <- trExpr2 e2
  return $ App e1' e2'

trExpr2 (Ty.Let bg e) = do
  pushBind
  ci <- getCi
  let (es, iss) = bg
      es' = map (\(n, _, alts) -> (n, alts)) es
      is = concat iss
      vdefs = dsgBs [] (es' ++ is) ci
  transVdefs vdefs
  b' <- popBind
  e' <- trExpr2 e
  return $ Let b' e'

trExpr2 (Ty.Const (n :>: sc)) = do
  qt <- freshInst' sc
  return $ Var $ TermVar n qt

trExpr2 expr = error $ "Non-exaustive patterns in trExpr2: " ++ show expr

dsgBs :: [(Id, Pat.Expression)]
      -> [(Id, [([Ty.Pat], Ty.Expr)])] -> ConstructorInfo
      -> [(Id, Pat.Expression)]

dsgBs vds [] _ = vds
dsgBs vds (impl:is) ci = dsgBs (desbs impl:vds) is ci
  where
    desbs (n, alts) = (n, dsgAlts n (cnvalts alts) ci)

dsgAlts ::
  Id -> [([Ty.Pat], Pat.Expression)] -> ConstructorInfo -> Pat.Expression

dsgAlts n alts@((pats,_):_) ci =
  let
    k = length pats
    us = [Pat.mkVar n i| i <- [1..k]]
    alts' = map (\(pats, e) -> (map wild2Var pats, e)) alts

    wild2Var :: Ty.Pat -> Ty.Pat
    wild2Var Ty.PWildcard      = Ty.PVar "_"
    wild2Var (Ty.PCon as pats) = Ty.PCon as (map wild2Var pats)
    wild2Var pat               = pat

    e = Pat.reduceMatch ci n k us alts' eFail

    eFail = Pat.OtherExpression (Ty.Var "Prim.FAIL")
  in
   Pat.Lambda us e

dsgAlts _ [] _ = error "dsgAlts: must not occur"

cnvalts :: [([Ty.Pat], Ty.Expr)] -> [([Ty.Pat], Pat.Expression)]
cnvalts = fmap (\(pats, e) -> (pats, Pat.OtherExpression e))
