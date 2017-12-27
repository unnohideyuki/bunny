module DictPass where

import           Core
import           Symbol
import           Types
import           Typing                     (Pred (..), Qual (..), Subst, apply,
                                             mgu, nullSubst, tv, (@@))

import           Control.Monad.State.Strict (State, get, put, runState)
import           Data.List                  (nub)
import           Data.Maybe                 (fromMaybe)
import           Debug.Trace

-- normalize Qualifier of Qual Type
normQTy :: Qual Type -> Qual Type
normQTy (qs :=> t) = let
  tvs = tv t
  qs' = filter (\(IsIn _ (TVar x)) -> elem x tvs) (nub qs)
  in
   qs' :=> t

tcBind :: Bind -> Bind
tcBind (Rec bs) = Rec $ map tcbind bs
  where
    tcbind (v@(TermVar n qt@(qs :=> _)), e)
      | isOVExpr e = (v, e)
      | otherwise =
        let st = mkTcState n qs
            (e', _) = runState (tcExpr e qt) st
        in
         if null qs then (v, e')
         else (v, Lam (mkVs n qs) e')
    tcbind _ = error "tcbind: must not occur."

isOVExpr :: Expr -> Bool -- whether (#overloaded# a b) form or not
isOVExpr (App (App (Var (TermVar "#overloaded#" _)) _) _) = True
isOVExpr _                                                = False

mkVs :: Id -> [Pred] -> [Var]
mkVs n ps = [TermVar (n ++ ".DARG" ++ show i) ([] :=> TGen (-2))
            | (i, _) <- zip [(0::Int)..] ps]

data TcState = TcState { tcName  :: Id
                       , tcPs    :: [Pred]
                       , tcSubst :: Subst
                       }
               deriving Show

type TC a = State TcState a

getSubst :: TC Subst
getSubst = do
  st <- get
  return $ tcSubst st

putSubst :: Subst -> TC ()
putSubst s = do
  st <- get
  put st{tcSubst = s}

extSubst :: Subst -> TC ()
extSubst s' = do
  s <- getSubst
  putSubst (s'@@s)

-- it is equivalent to Typing.unify
unify' :: Type -> Type -> TC ()
unify' t1 t2 = do s <- getSubst
                  u <- mgu (apply s t1) (apply s t2)
                  extSubst u

getName :: TC Id
getName = do
  st <- get
  return $ tcName st

getPs :: TC [Pred]
getPs = do
  st <- get
  return $ tcPs st

getTy :: Expr -> TC (Qual Type)
-- TODO: quantify here is ok?
getTy (Var (TermVar _ qt)) = return qt
getTy (Lit (LitInt _ t)) = return ([] :=> t)
getTy (Lit (LitChar _ t)) = return ([] :=> t)
getTy (Lit (LitFrac _ t)) = return ([] :=> t)
getTy (Lit (LitStr _ t)) = return ([] :=> t)

getTy (App f e) = do
  (qe :=> te) <- getTy e
  (qf :=> tf) <- getTy f
  return $ normQTy ((qe++qf) :=> tyapp tf te)

getTy (Lam vs e) = do
  (qe :=> te) <-  getTy e
  let
    qv = concatMap (\(TermVar _ (ps :=> _)) -> ps)  vs
    ts = map (\(TermVar _ (_ :=> t')) -> t')  vs
  return $ normQTy ((qe++qv) :=> foldr fn te ts)

getTy (Case _ _ as) = do
  qts <- mapM (getTy.(\(_,_,e) -> e)) as
  let
    qs = concatMap (\(q :=> _) -> q) qts
    ts = map (\(_ :=> t') -> t') qts
  t <- unifyTs ts
  return $ normQTy (qs :=> t)

getTy (Let _ e) = getTy e

getTy e = fail $ "Non-Exaustive Patterns in getTy: " ++ show e

tyapp :: Type -> Type -> Type
tyapp ta tb =
  let
    a = TVar (Tyvar "a" Star)
    tf = tb `fn` a
    s = fromMaybe (error "do not unified in tyapp")
        (mgu ta tf)
  in
   apply s a

unifyTs :: Monad m => [Type] -> m Type
unifyTs [t] = return t
unifyTs (t:ts) = do
  t' <- unifyTs ts
  s <- mgu t t'
  return $ apply s t
unifyTs [] = fail "Non-exhaustive patterns in uniftyTs."

lookupDictArg :: (Id, Tyvar) -> TC (Maybe Var)
lookupDictArg (c, x) = do
  n <- getName
  s <- getSubst
  ps' <- getPs
  let ps = apply s ps'
      d = zip (map (\(IsIn i t) -> (i, t)) ps) [(0::Int)..]
      (TVar y) = apply s (TVar x)
      ret = case lookup (c, TVar y) d of
        Nothing -> Nothing
        Just j -> Just $ TermVar (n ++ ".DARG" ++ show j) ([] :=> TGen (-2))
  return ret

mkTcState :: Id -> [Pred] -> TcState
mkTcState n ps = TcState{tcName=n, tcPs=ps, tcSubst=nullSubst}

tcExpr :: Expr -> Qual Type -> TC Expr

tcExpr e@(Var (TermVar n (qv :=> t'))) (_ :=> t)
  | null qv || isTVar t' e || isArg n {- todo:too suspicious! -} = return e
  | otherwise = do
  unify' t' t
  s <- getSubst
  let mkdicts [] ds = return ds
      mkdicts (IsIn n2 (TVar x) : qs) ds =
        case apply s (TVar x) of
          (TCon (Tycon n1 _)) -> mkdicts qs (Var (DictVar n1 n2) : ds)
          _ -> do v <- lookupDictArg (n2, x)
                  case v of
                    Nothing -> error ("Error: dictionary not found: "
                                      ++ n ++ ", " ++ show (x,s))
                    Just v' -> mkdicts qs (Var v' : ds)
      mkdicts _ _ = error "mkdicts: must not occur"

  dicts <- mkdicts qv []
  return $ foldl App e dicts
  where isTVar x@(TVar _) y | notFunTy x = True
                            | otherwise = trace (show y) True
        isTVar x y | not (notFunTy x) = False
                   | otherwise         = trace (show y) False
        notFunTy (TAp (TAp (TCon (Tycon "(->)" _)) _) _) = False
        notFunTy _                                       = True
        isArg ('_':_) = True
        isArg _ = False

tcExpr e@(Lit _) _ = return e

tcExpr (App e f) (ps :=> t) = do
  (qe :=> te) <- getTy e
  (qf :=> tf) <- getTy f
  unify' te (tf `fn` t)
  s <- getSubst
  e1 <- tcExpr e (normQTy ((ps++qe++qf) :=> apply s te))
  e2 <- tcExpr f (normQTy ((ps++qe++qf) :=> apply s tf))
  return $ App e1 e2

tcExpr (Lam vs e) (qs :=> t) = do
  (qe :=> te) <- getTy e
  let
    ts = map (\(TermVar _ qt) -> case qt of {_ :=> x -> x})  vs
    t' = foldr fn te ts
  unify' t t'
  e' <- tcExpr e (normQTy ((qs++qe) :=> te))
  return (Lam vs e')

tcExpr (Let bs e) qt = do
  let bs' = tcBind bs
  e' <- tcExpr e qt
  return $ Let bs' e'

-- TODO: type-check scrut
tcExpr e@(Case scrut v as) (ps :=> t) = do
  (qe :=> te) <- getTy e
  unify' t te
  s <- getSubst
  let
    t' = apply s te
    tcAs [] _ = return []
    tcAs ((ac, vs, x):as'') qt = do
      e' <- tcExpr x qt
      as' <- tcAs as'' qt
      return $ (ac, vs, e') : as'
  as' <- tcAs as (normQTy ((ps++qe) :=> t'))
  return $ Case scrut v as'

tcExpr e _ =
  let
    s = take 30 (show e)
  in
   trace ("warning: temporary dummy tcExpr: " ++ s ++ "...") $ return e
