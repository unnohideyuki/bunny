module DictPass where

import           Core
import           Symbol
import           Types
import           Typing                     (Pred (..), Qual (..), Scheme (..),
                                             Subst, apply, mgu)

import           Control.Monad.State.Strict (State, get, put, runState)
import           Debug.Trace

getTy :: Expr -> Qual Type

-- TODO: quantify here is ok?
getTy (Var (TermVar _ qt)) = qt
getTy (Lit (LitInt _ t)) = ([] :=> t)
getTy (Lit (LitChar _ t)) = ([] :=> t)
getTy (Lit (LitFrac _ t)) = ([] :=> t)
getTy (Lit (LitStr _ t)) = ([] :=> t)

getTy (App e f) =
  let (_, te) = case getTy e of {q :=> t -> (q, t)}
      (_, tf) = case getTy f of {q :=> t -> (q, t)}
  in [] :=> tyapp te tf -- empty qualifier is OK here, see Note #p.244.

getTy (Lam vs e) = [] :=> (foldr fn te ts)
  where
    te = case getTy e of {_ :=> t -> t}
    ts = map (\(TermVar _ (_ :=> t')) -> t')  vs

getTy (Case _ _ as) =
  let
    ts = map ((\(_ :=> t) -> t).getTy.(\(_,_,e) -> e)) as
    t = case unifyTs ts of
      Just t' -> t'
      Nothing -> error $ "type-error in getTy for Case: " ++ show ts
  in
   [] :=> t

getTy (Let _ e) = getTy e

getTy e = error $ "Non-Exaustive Patterns in getTy: " ++ show e

tyapp :: Type -> Type -> Type
tyapp ta tb =
  let
    a = TVar (Tyvar "a" Star)
    tf = tb `fn` a
    s = case mgu ta tf of
      Just s' -> s'
      Nothing -> error "do not unified in tyapp"
  in
   apply s a

unifyTs :: Monad m => [Type] -> m Type
unifyTs [t] = return t
unifyTs (t:ts) = do
  t' <- unifyTs ts
  s <- mgu t t'
  return $ apply s t
unifyTs [] = fail $ "Non-exhaustive patterns in uniftyTs."

tcBind :: Bind -> Bind
tcBind (Rec bs) = Rec $ map tcbind bs
  where
    tcbind (v@(TermVar n qt@(qs :=> t)), e)
      | isOVExpr e = (v, e)
      | otherwise =
        let st = mkTcState n qs
            t' = case qt of
              (_ :=> x) -> x
            (e', _) = trace (show (n, qt, e)) $ runState (tcExpr e t') st
        in
         if null qs then (v, e')
         else (v, Lam (mkVs n qs) e')

isOVExpr :: Expr -> Bool -- whether (#overloaded# a b) form or not
isOVExpr (App (App (Var (TermVar "#overloaded#" _)) _) _) = True
isOVExpr _                                                = False

mkVs n ps = [TermVar (n ++ ".DARG" ++ show i) ([] :=> (TGen (-2)))
            | (i, _) <- zip [0..] ps]

data TcState = TcState { tc_name :: Id
                       , tc_ps   :: [Pred]
                       }
               deriving Show

type TC a = State TcState a

getName :: TC Id
getName = do
  st <- get
  return $ tc_name st

getPs :: TC [Pred]
getPs = do
  st <- get
  return $ tc_ps st

putPs :: [Pred] -> TC ()
putPs ps = do
  st <- get
  put st{tc_ps = ps}

lookupDictArg :: (Id, Tyvar) -> Subst -> TC (Maybe Var)
lookupDictArg (c, tv) s = do
  ps <- getPs
  n <- getName
  let d = zip (map (\(IsIn i t) -> (i, t)) ps) [0..]
      ret = case lookup (c, TVar tv) d of
        Nothing -> case lookup (c, apply s (TVar tv)) d of
                     Nothing -> trace (show (c, tv, d, s)) Nothing
                     Just j -> Just $ TermVar (n ++ ".DARG" ++ show j) ([] :=> (TGen (-2)))
        Just j -> Just $ TermVar (n ++ ".DARG" ++ show j) ([] :=> (TGen (-2)))
  return ret

mkTcState :: Id -> [Pred] -> TcState
mkTcState n ps = TcState{tc_name=n, tc_ps=ps}

tcExpr :: Expr -> Type -> TC Expr

-- tcExpr e@(Var v@(TermVar _ (qv :=> tv))) t = do
tcExpr e@(Var v@(TermVar _ (qv :=> t'))) t
  | null qv || notFunTy t' = return e
  | otherwise = do
  n <- getName
  ps <- getPs
  let s = case mgu t' t of
        Just s' -> s'
        Nothing -> error "fatal: do not unified in tcExpr'."

      mkdicts [] ds = return ds
      mkdicts ((IsIn n2 (TVar tv)):qs) ds = do
        case apply s (TVar tv) of
          (TCon (Tycon n1 _)) -> mkdicts qs ((Var (DictVar n1 n2)):ds)
          _ -> do v <- lookupDictArg (n2, tv) s
                  case v of
                    Nothing -> error ("Error: dictionary not found: " ++ show (tv,s))
                    Just v' -> mkdicts qs ((Var v'):ds)

  dicts <- mkdicts qv []

  let appdicts e []     = e
      appdicts e (d:ds) = appdicts (App e d) ds

  return $ appdicts e dicts
  where notFunTy (TAp (TAp (TCon (Tycon "(->)" _)) _) _) = False
        notFunTy _ = True

tcExpr e@(Lit _) _ = return e

tcExpr (App e f) t = do
  let (_ :=> te) = getTy e
      (_ :=> tf) = getTy f
      t' = tf `fn` t
      s = case mgu te t' of
            Just s' -> s'
            Nothing -> error $ "Error in mgu" ++ show(te, t')
      te' = apply s te
      tf' = apply s tf
  e1 <- tcExpr e te'
  e2 <- tcExpr f tf'
  return $ App e1 e2

tcExpr (Lam vs e) t =
  let
    te = case getTy e of {_ :=> t -> t}
    ts = map (\(TermVar _ qt) -> case qt of {_ :=> t -> t})  vs
    t' = foldr fn te ts
  in
   case mgu t t' of
     Just s -> do
       ps <- getPs
       let ps' = map (apply s) ps
       putPs ps'
       e' <- tcExpr e te
       putPs ps
       return (Lam vs e')
     Nothing -> error $ "type-check error in tcExpr: " ++ show (e, t)

tcExpr (Let bs e) t = do
  let bs' = tcBind bs
  e' <- tcExpr e t
  return $ Let bs' e'

-- TODO: type-check scrut
tcExpr e@(Case scrut v as) t =
  let
    te = case getTy e of { _ :=> x -> x}
    s = case mgu t te of
      Nothing -> error $ "type-error in tcExpr for Case: " ++ show (t, t')
      Just s' -> s'
    t' = apply s te

    tcAs [] _ = return []
    tcAs ((ac, vs, e):as) t = do
      e' <- tcExpr e t
      as' <- tcAs as t
      return $ (ac, vs, e') : as'
  in
   do as' <- tcAs as t'
      return $ Case scrut v as'

tcExpr e _ =
  let
    s = take 30 (show e)
  in
   trace ("warning: temporary dummy tcExpr: " ++ s ++ "...") $ return e
