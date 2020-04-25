module DictPass where

import           Core
import           PPTypes
import           Symbol
import           Types
import           Typing                     (Assump (..), ClassEnv (..),
                                             Pred (..), Qual (..), Scheme (..),
                                             Subst, apply, find, mgu, nullSubst,
                                             super, tv, (@@))

import           Control.Monad
import           Control.Monad.State.Strict (State, get, put, runState)
import           Data.List                  (nub)
import           Data.List.Split            (splitOn)
import           Data.Maybe                 (fromMaybe)
import           Debug.Trace

-- normalize Qualifier of Qual Type
normQTy :: Qual Type -> Qual Type
normQTy (qs :=> t) = let
  tvs = tv t
  qs' = filter (\(IsIn _ (TVar x)) -> elem x tvs) (nub qs)
  in
   qs' :=> t

tcBind :: Bind -> ClassEnv -> Maybe TcState -> Bind
tcBind (Rec bs) ce maybest = Rec $ map tcbind bs
  where
    tcbind (v@(TermVar n qt@(qs :=> _)), e)
      | isOVExpr e = (v, e)
      | otherwise =
        let pss = (zip qs (repeat n))
            st = case maybest of
              Just st' -> let pss' = tcPss st'
                              subst = tcSubst st'
                              num = tcNum st'
                          in mkTcState ce (pss ++ pss') subst num
              Nothing -> mkTcState ce pss nullSubst 0
            (e', _) = runState (tcExpr e qt) st
        in
         if null qs then (v, e')
         else (v, Lam (mkVs n qs) e')
    tcbind _ = error "tcbind: must not occur."

isOVExpr :: Expr -> Bool -- whether (#overloaded# a b) form or not
isOVExpr (App (App (Var (TermVar "#overloaded#" _)) _) _) = True
isOVExpr _                                                = False

mkVs :: Id -> [Pred] -> [Var]
mkVs n ps = [TermVar (n ++ ".DARG" ++ show i) ([] :=> TGen 99)
            | (i, _) <- zip [(0::Int)..] ps]

data TcState = TcState { tcCe           :: ClassEnv
                       , tcPss          :: [(Pred, Id)]
                       , tcSubst        :: Subst
                       , tcNum          :: Int
                       , tcIntegerTVars :: [Type]
                       }
               deriving Show

type TC a = State TcState a

newNum :: TC Int
newNum = do
  st <- get
  let n = tcNum st
  put st{tcNum = n + 1}
  return n

getSubst :: TC Subst
getSubst = do st <- get
              return $ tcSubst st

putSubst :: Subst -> TC ()
putSubst s = do st <- get
                put st{tcSubst = s}

extSubst :: Subst -> TC ()
extSubst s' = do s <- getSubst
                 putSubst (s'@@s)

-- it is equivalent to Typing.unify
unify' :: Type -> Type -> TC ()
unify' t1 t2 = do s <- getSubst
                  u <- mgu (apply s t1) (apply s t2)
                  extSubst u

getPss :: TC [(Pred, Id)]
getPss = do st <- get
            return $ tcPss st

getCe :: TC ClassEnv
getCe = tcCe <$> get


getTy :: Expr -> TC (Qual Type)
getTy (Var (TermVar _ qt)) = return qt
getTy (Lit (LitChar _ qt)) = return qt
getTy (Lit (LitFrac _ qt)) = return qt
getTy (Lit (LitStr  _ qt)) = return qt

getTy e@(Lit (LitInt  _ qt@(_ :=> v))) = do
  st <- get
  let tvars = tcIntegerTVars st
      st' = st{tcIntegerTVars = (v:tvars)}
  put st'
  return qt

getTy (App f e) = do
  (qf :=> tf) <- getTy f
  (qe :=> te) <- getTy e
  t <- tyapp tf te
  return $ normQTy ((qe++qf) :=> t)

getTy (Lam vs e) = do
  (qe :=> te) <-  getTy e
  let
    qv = concatMap (\(TermVar _ (ps :=> _)) -> ps)  vs
    ts = map (\(TermVar _ (_ :=> t')) -> t')  vs
  return $ normQTy ((qe++qv) :=> foldr fn te ts)

getTy (Case scrut _ as) = do
  qts <- mapM (getTy.(\(_,_,e) -> e)) as
  let
    qs = concatMap (\(q :=> _) -> q) qts
    ts = map (\(_ :=> t') -> t') qts
  t <- unifyTs ts
  return $ normQTy (qs :=> t)

getTy (Let _ e) = getTy e

getTy e = fail $ "Non-Exaustive Patterns in getTy: " ++ show e

tyScrut s as = do
  qt' :=> t <- getTy s
  ts <- mapM altty as
  t' <- unifyTs (t:ts)
  return $ qt' :=> t'
  where altty (DataAlt (DataCon n vs qt), _, _) = do
          let c = Var (TermVar n qt)
              es = map Var vs
              f x []     = x
              f x (y:ys) = f (App x y) ys
          _ :=> t <- getTy (f c es)
          return t

        altty (LitAlt l, _, _) = return $
                                 case l of
                                   LitInt  _ ( _ :=> t) -> t
                                   LitChar _ ( _ :=> t) -> t
                                   LitFrac _ ( _ :=> t) -> t
                                   LitStr  _ ( _ :=> t) -> t

        altty _ = do n <- newNum
                     return $ TVar (Tyvar ("a" ++ show n) Star)

tyapp :: Type -> Type -> TC Type
tyapp ta tb = do
  n <- newNum
  let a = TVar (Tyvar ("a" ++ show n) Star)
      tf = tb `fn` a
      s = fromMaybe
          (error $ "do not unified in tyapp:\n\t" ++ show ta ++
           "\n\t" ++ show tf
          )
          (mgu ta tf)
  extSubst s
  return $ apply s a

unifyTs :: [Type] -> TC Type
unifyTs [t] = return t
unifyTs (t:ts) = do
  t' <- unifyTs ts
  unify' t t'
  s <- getSubst
  return $ apply s t
unifyTs [] = fail "Non-exhaustive patterns in uniftyTs."

lookupDictArg :: (Id, Tyvar) -> TC (Maybe Var)
lookupDictArg (c, x) = do
  -- trace ("lookupDictArg " ++ show (c, x)) $ return ()
  s <- getSubst
  pss <- getPss
  ce <- getCe
  let d = zip (map (\((IsIn i t), _) -> (i, apply s t)) pss) [(0::Int)..]
      (TVar y) = case apply s (TVar x) of
        (TVar x') -> (TVar x')
        x'        -> error $ "Irrefutable pattern failed " ++ show (x', ce)

      lookupDict (k, tv) (((c, tv'), i):d')
        | tv == tv' && (k == c  || k `elem` super ce c) = Just i
        | otherwise = lookupDict (k, tv) d'
      lookupDict _ [] = Nothing

      ret = case lookupDict (c, TVar y) d of
        Nothing -> Nothing
        Just j  -> let (_, n) = pss !! j
                   in Just $ TermVar (n ++ ".DARG" ++ show j) ([] :=> TGen 100)

  -- trace (show (d, (apply s (TVar x)), ret)) $ return ()
  return ret

mkTcState :: ClassEnv -> [(Pred, Id)] -> Subst -> Int -> TcState
mkTcState ce pss subst num =
  TcState{tcCe=ce, tcPss=pss, tcSubst=subst, tcNum=num, tcIntegerTVars=[]}

tcExpr :: Expr -> Qual Type -> TC Expr

tcExpr e@(Var (TermVar n (qv :=> t'))) qt -- why ignore qs?
  | null qv || isTVar t' e || isArg n {- todo:too suspicious! -} = return e
  | otherwise = findApplyDict e (qv :=> t') qt
  where isTVar x@(TVar _) y = True
        isTVar x y          = False
        notFunTy (TAp (TAp (TCon (Tycon "(->)" _)) _) _) = False
        notFunTy _                                       = True
        isArg ('_':_) = True
        isArg _       = False

        findApplyDict e (qv :=> t') (_ :=> t) = do
          unify' t' t
          s <- getSubst
          itvars <- tcIntegerTVars <$> get
          let itvars' = fmap (apply s) itvars
          let  mkdicts [] ds = return ds
               mkdicts (IsIn n2 (TVar x) : qs) ds =
                 case apply s (TVar x) of
                   (TCon (Tycon n1 _)) -> mkdicts qs (Var (DictVar n1 n2) : ds)
                   y | y `elem` itvars'
                       -> mkdicts qs (Var (DictVar "Prelude.Integer" n2) : ds)
                     | otherwise
                       -> do v <- lookupDictArg (n2, x)
                             case v of
                               Nothing -> error ("Error: dictionary not found: "
                                                 ++ n ++ ", " ++ show (x,n2,y,itvars))
                               Just v' -> mkdicts qs (Var v' : ds)
               mkdicts _ _ = error "mkdicts: must not occur"
          dicts <- mkdicts qv [] -- mkdicts returns dictionaries in reverse order
          return (foldr (flip App) e dicts)
            {-
            where
              appliedQv :: [Expr] -> TC (Maybe (Qual Type))
              appliedQv [(Var (DictVar iname _))] = do
                let mname = iname ++ "%I." ++ last (splitOn "." n)
                as <- tcAs <$> get
                let maybe_sc = find mname as
                case maybe_sc of
                  Just (Forall _ qt) -> return (Just qt)
                  Nothing            -> return Nothing
              appliedQv _ = return Nothing
            -}

tcExpr e@(Lit _) _ = do return e


tcExpr (App e f) (ps :=> t) = do
  (qe :=> te) <- getTy e
  (qf :=> tf) <- getTy f
  unify' te (tf `fn` t)
  s <- getSubst
  e1 <- tcExpr e (normQTy ((ps++qe++qf) :=> apply s te))
  e2 <- tcExpr f (normQTy ((ps++qe++qf) :=> apply s tf))
  return $ App e1 e2

tcExpr e@(Lam vs ebody) (qs :=> t) = do
  _ :=> t' <- getTy e
  unify' t' t
  qt <- getTy ebody
  s <- getSubst
  ebody' <- tcExpr ebody (apply s qt)
  return $ Lam vs ebody'

tcExpr (Let bs e) qt = do
  st <- get
  let bs' = tcBind bs (tcCe st) (Just st)
  e' <- tcExpr e qt
  return $ Let bs' e'

tcExpr e@(Case scrut v as) (ps :=> t) = do
  _ <- tyScrut scrut as
  qe :=> te <- getTy e
  unify' t te
  s <- getSubst
  let
    qt' = normQTy ((ps++qe) :=> apply s te)
    tcAs qt (ac, vs, x) = do
      e' <- tcExpr x qt
      return (ac, vs, e')
  as' <- mapM (tcAs qt') as
  return $ Case scrut v as'


tcExpr e _ =
  let
    s = take 30 (show e)
  in
   trace ("warning: temporary dummy tcExpr: " ++ s ++ "...") $ return e
