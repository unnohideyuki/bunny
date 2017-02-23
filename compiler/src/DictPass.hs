module DictPass where

import Core
import Types
import Typing (Qual(..), Pred(..), mgu, apply, Scheme(..), tv, quantify)
import Symbol

import Data.List
import Control.Monad (when)
import Control.Monad.State.Strict (State, state, get, put, runState)
import Debug.Trace

getTy :: Expr -> Qual Type

-- TODO: quantify here is ok?
getTy (Var (TermVar _ qt)) =
  case quantify (tv qt) qt of
    Forall _ qt' -> qt'

getTy (Lit (LitInt _ t)) = ([] :=> t)
getTy (Lit (LitChar _ t)) = ([] :=> t)
getTy (Lit (LitFrac _ t)) = ([] :=> t)
getTy (Lit (LitStr _ t)) = ([] :=> t)

getTy (App e f) =
  let (qe, te) = case getTy e of {q :=> t -> (q, t)}
      (qf, tf) = case getTy f of {q :=> t -> (q, t)}
  in [] :=> tyapp te tf -- empty qualifier is OK here, see Note #p.244.

getTy (Lam vs e) = [] :=> (foldr fn te ts)
  where
    te = case getTy e of {_ :=> t -> t}
    quantifyType qt = case quantify (tv qt) qt of
      Forall _ (_ :=> t) -> t
    ts = map (\(TermVar _ qt) -> quantifyType qt)  vs

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
    tf = tb `fn` (TGen (-1))
    s = case unifier ta tf of
      Just s' -> s'
      Nothing -> error "do not unified in tyapp"
  in
   subst s (TGen (-1))


type Unifier = [(Int, Type)]

unifier :: Type -> Type -> Maybe Unifier
unifier (TAp l r) (TAp l' r') = do s1 <- unifier l l'
                                   s2 <- unifier (subst s1 r) (subst s1 r')
                                   return (s1 ++ s2)
unifier (TGen _) (TGen _) = return []
unifier (TGen i) t = return [(i, t)]
unifier t (TGen i) = return [(i, t)]
unifier (TCon tc1) (TCon tc2) | tc1 == tc2 = return []
unifier t1 t2 = trace ("warning: unifier: " ++ show (t1, t2)) $ return []

unifyTs :: [Type] -> Maybe Type
unifyTs [t] = Just t
unifyTs (t:ts) =
  let
    maybe_s = case unifyTs ts of
      Nothing -> Nothing
      Just t' -> unifier t t'
  in
    case maybe_s of
      Nothing -> Nothing
      Just s' -> Just $ subst s' t

subst :: Unifier -> Type -> Type
subst s (TGen i) = case lookup i s of
  Just t -> t
  Nothing -> TGen i
subst s (TAp l r) = TAp (subst s l) (subst s r)
subst _ t = t

tcBind :: Bind -> Bind
tcBind (Rec bs) = Rec $ map tcbind bs
  where
    tcbind (v@(TermVar n qt@(qs :=> t)), e)
      | isOVExpr e = (v, e)
      | otherwise = 
        let st = mkTcState n qs
            t' = case quantify (tv qt) qt of
              Forall _ (qs :=> x) -> x
            (e', _) = runState (tcExpr e t') st
        in
         if null qs then (v, e')
         else (v, Lam (mkVs n qs) e')

isOVExpr :: Expr -> Bool -- whether (#overloaded# a b) form or not
isOVExpr (App (App (Var (TermVar "#overloaded#" _)) _) _) = True
isOVExpr _ = False

mkVs n ps = [TermVar (n ++ ".DARG" ++ show i) ([] :=> (TGen (-2)))
            | (i, _) <- zip [0..] ps]

data TcState = TcState { tc_name :: Id
                       , tc_ps :: [Pred]
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

lookupDictArg :: (Id, Int) -> TC (Maybe Var)
lookupDictArg (c, i) = do
  ps <- getPs
  n <- getName
  let d = zip (map (\(IsIn i t) -> (i, t)) ps) [0..]
      ret = case lookup (c, (TGen i)) d of
        Nothing -> trace (show (n, c, i, ps)) Nothing
        Just j -> Just $ TermVar (n ++ ".DARG" ++ show j) ([] :=> (TGen (-2)))
  return ret
      
mkTcState :: Id -> [Pred] -> TcState
mkTcState n ps = TcState{tc_name=n, tc_ps=ps}

tcExpr :: Expr -> Type -> TC Expr

-- tcExpr e@(Var v@(TermVar _ (qv :=> tv))) t = do
tcExpr e@(Var v@(TermVar _ qt)) t = do
  let qt' = case quantify (tv qt) qt of
        Forall _ x -> x
      (qv :=> t') = qt'
  n <- getName
  ps <- getPs
  let s = case unifier t' t of
        Just s' -> s'
        Nothing -> error "fatal: do not unified in tcExpr'."

      -- todo: support the case when length qv > 1
      (c, i) = case qv of
        [IsIn n (TGen j)] -> (n, j)
        _ -> ("", -1)

      mkdps e t =
        let
          n1 = case t of
            (TCon (Tycon n _)) -> n
            _ -> error $ "illegal type for dictionary: " ++ show t

          v = case e of { Var v -> v }

          n2 = case v of
            Core.TermVar _ ([IsIn n _] :=> _) -> n

          dps = App e (Var (DictVar n1 n2))
        in
         trace (show dps) dps

  if null qv then return e
    else
    case lookup i s of
      Just t -> return $ mkdps e t
      Nothing -> do v <- lookupDictArg (c, i)
                    case v of
                      Nothing -> trace ("Error: dictionary not found: " ++ show i) $ return e
                      Just v' -> return $ App e (Var v')

tcExpr e@(Lit _) _ = return e

tcExpr (App e f) t = do
  let (_ :=> te) = getTy e
      (_ :=> tf) = getTy f
      t' = tf `fn` t
      s = case unifier te t' of {Just s' -> s'}
      te' = subst s te
      tf' = subst s tf
  e1 <- tcExpr e te'
  e2 <- tcExpr f tf'
  return $ App e1 e2

tcExpr (Lam vs e) t =
  let
    te = case getTy e of {_ :=> t -> t}
    ts = map (\(TermVar _ qt) -> case qt of {_ :=> t -> t})  vs
    t' = foldr fn te ts
  in
   case unifier t t' of
     Just s -> do
       -- todo: subst ts
       e' <- tcExpr e (subst s te)
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
    s = case unifier t te of
      Nothing -> error $ "type-error in tcExpr for Case: " ++ show (t, t')
      Just s' -> s'
    t' = subst s te

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
