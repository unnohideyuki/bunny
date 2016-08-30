module DictPass where

import Core
import Types
import Typing (Qual(..), mgu, apply)
import Symbol

import Data.List
import Debug.Trace

getTy :: Expr -> Qual Type

getTy (Var v) = case v of
  TermVar _ t -> t

getTy (Lit (LitInt _ t)) = ([] :=> t)
getTy (Lit (LitChar _ t)) = ([] :=> t)
getTy (Lit (LitFrac _ t)) = ([] :=> t)
getTy (Lit (LitStr _ t)) = ([] :=> t)

getTy (App e f) =
  let (qe, te) = case getTy e of {q :=> t -> (q, t)}
      (qf, tf) = case getTy f of {q :=> t -> (q, t)}
  in [] :=> tyapp te tf -- empty qualifier is OK here, see Note #p.244.

tyapp :: Type -> Type -> Type
tyapp ta tb =
  let
    tf = tb `fn` (TGen 100)
    s = case unifier ta tf of
      Just s' -> s'
      Nothing -> error "do not unified in tyapp"
  in
   subst s (TGen 100)


type Unifier = [(Int, Type)]

unifier :: Type -> Type -> Maybe Unifier
unifier (TAp l r) (TAp l' r') = do s1 <- unifier l l'
                                   s2 <- unifier (subst s1 r) (subst s1 r')
                                   return (s1 ++ s2)
unifier (TGen _) (TGen _) = return []
unifier (TGen i) t = return [(i, t)]
unifier t (TGen i) = return [(i, t)]
unifier (TCon tc1) (TCon tc2) | tc1 == tc2 = return []

subst :: Unifier -> Type -> Type
subst s (TGen i) = case lookup i s of
  Just t -> t
  Nothing -> TGen i
subst s (TAp l r) = TAp (subst s l) (subst s r)
subst _ t = t

