module DictPass where

import Core
import Types
import Typing (Qual(..), Pred(..), mgu, apply)
import Symbol

import Data.List
import Control.Monad (when)
import Control.Monad.State.Strict (State, state, get, put, runState)
import Debug.Trace

getTy :: Expr -> Qual Type

getTy (Var (TermVar _ t)) = t

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
    ts = map (\(TermVar _ qt) -> case qt of {_ :=> t -> t})  vs

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
    tcbind (v@(TermVar n (qs :=> t)), e)
      | isOVExpr e = (v, e)
      | otherwise = 
        let st = mkTcState n qs
            (e', _) = runState (tcExpr e t) st
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

lookupDictArg :: Int -> TC (Maybe Var)
lookupDictArg i = do
  ps <- getPs
  n <- getName
  let d = zip (map (\(IsIn _ t) -> t) ps) [0..]
      ret = case lookup (TGen i) d of
        Nothing -> Nothing
        Just j -> Just $ TermVar (n ++ ".DARG" ++ show j) ([] :=> (TGen (-2)))
  return ret
      
mkTcState :: Id -> [Pred] -> TcState
mkTcState n ps = TcState{tc_name=n, tc_ps=ps}

tcExpr :: Expr -> Type -> TC Expr

tcExpr e@(Var v@(TermVar _ (qv :=> tv))) t = do
  n <- getName
  ps <- getPs
  let s = case unifier tv t of
        Just s' -> s'
        Nothing -> error "fatal: do not unified in tcExpr'."

      -- todo: support the case when length qv > 1
      i = case qv of
        [IsIn _ (TGen j)] -> j
        _ -> -1

      mkdps e t =
        let
          dictname = case t of
            (TCon (Tycon n _)) -> n
            _ -> error $ "illegal type for dictionary: " ++ show t

          v = case e of { Var v -> v }
        in
         (Dps v (Dict dictname)) 

  when (ps /= []) $ trace (show (n, ps, e, t, i, s, lookup i s)) $ return ()

  if null qv then return e
    else
    case lookup i s of
      Just t -> return $ mkdps e t
      Nothing -> do v <- lookupDictArg i
                    case v of
                      Nothing -> error "dictionary not found."
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

tcExpr e _ =
  let
    s = take 30 (show e)
  in
   trace ("warning: temporary dummy tcExpr: " ++ s ++ "...") $ return e
