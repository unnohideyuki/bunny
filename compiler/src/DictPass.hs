module DictPass where

import Core
import Types
import Typing (Qual(..), Pred(..), mgu, apply)
import Symbol

import Data.List
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

    unifTy [t] = t
    unifTy (t:ts) =
      let
        s = case unifier t (unifTy ts) of
          Just s -> s
          Nothing -> error $ "type-error in getTy(for Case):unifty: " ++ show (t, ts)
      in
       subst s t
  in
   [] :=> unifTy ts

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

subst :: Unifier -> Type -> Type
subst s (TGen i) = case lookup i s of
  Just t -> t
  Nothing -> TGen i
subst s (TAp l r) = TAp (subst s l) (subst s r)
subst _ t = t

{- tcExpr -- type-check expression
-}
tcExpr :: Expr -> Type -> Expr

tcExpr e@(Var v@(TermVar _ (qv :=> tv))) t = let
  s = case unifier tv t of
    Just s' -> s'
    Nothing -> error "fatal: do not unified in tcExpr."

  -- todo: support the case when length qv > 1
  i = case qv of
    [IsIn _ (TGen j)] -> j
    _ -> -1

  e' = case lookup i s of
    Nothing -> e
    Just t -> mkdps e t

  mkdps e t =
    let
      dictname = case t of
        (TCon (Tycon n _)) -> n
        _ -> error $ "illegal type for dictionary: " ++ show t

      v = case e of { Var v -> v }
    in
     (Dps v (Dict dictname)) 

  in
   e'

tcExpr e@(Lit _) _ = e

tcExpr (App e f) t =
  let
    (_ :=> te) = getTy e
    (_ :=> tf) = getTy f

    t' = tf `fn` t

    s = case unifier te t' of {Just s' -> s'}

    te' = subst s te
    tf' = subst s tf
  in
   App (tcExpr e te') (tcExpr f tf')

tcExpr e@(Lam _ _) t =
  let
    t' = case getTy e of { _ :=> x -> x }
  in
   case unifier t t' of
     Just _ -> e
     Nothing -> error $ "type-check error in tcExpr: " ++ show (e, t)

tcExpr (Let bs e) t =
  let
    bs' = tcBind bs
    e' = tcExpr e t
  in
   Let bs' e'


tcExpr e _ =
  let
    s = take 30 (show e)
  in
   trace ("warning: temporary dummy tcExpr: " ++ s ++ "...") e

tcBind :: Bind -> Bind
tcBind (Rec bs) = Rec $ map tcbind bs
  where
    tcbind (v@(TermVar _ ([] :=> t)), e) = (v, tcExpr e t)
    tcbind b@(v@(TermVar _ (q :=> _)), e)
      | isOVExpr e = b -- #overloaded#
      | otherwise  = trace ("Polymorphism: " ++ show v) b


isOVExpr :: Expr -> Bool -- whether (#overloaded# a b) form or not
isOVExpr (App (App (Var (TermVar "#overloaded#" _)) _) _) = True
isOVExpr _ = False
