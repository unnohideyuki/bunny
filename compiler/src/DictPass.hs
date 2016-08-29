module DictPass where

import Core
import Symbol

import Data.List
import Debug.Trace

getTy :: Expr -> Type

getTy (Var v) = case v of
  TermVar _ t -> t
  TypeVar _ _ -> error "must not occur."

getTy (Lit (LitInt _ t)) = t
getTy (Lit (LitChar _ t)) = t
getTy (Lit (LitFrac _ t)) = t
getTy (Lit (LitStr _ t)) = t

getTy (App e f) =
  let te = getTy e
      tf = getTy f
  in tyapp te tf


{-
extrDictTy t -- extract DictTys from a type
  return value :: ([Type], Type) -- list of DictTys and the remain.
-}
extrDictTy :: Type -> ([Type], Type)
extrDictTy t = extrdt t []
  where
    extrdt (FunTy dt@(DictTy _ _) rt) dts = extrdt rt (dt:dts)
    extrdt t dts = (dts, t)

tyVars :: Type -> [Id]
tyVars (TyVarTy (TypeVar n _)) = [n]
tyVars (TyVarTy (TermVar _ _)) = []
tyVars (AppTy t1 t2) = tyVars t1 `union` tyVars t2
tyVars (TyConApp _ ts) = concatMap tyVars ts
tyVars (FunTy e f) = tyVars e `union` tyVars f
tyVars (DictTy _ n) = [n]

tyapp :: Type -> Type -> Type
tyapp ta tb =
  let
    (te, tf) = case extrDictTy ta of
      (_, FunTy t1 t2) -> (t1, t2)
      _ -> error $ "not a FunTy: " ++ show ta

    {-
    s = getUnifier te tb
    tf' = applySubst s tf
    -}
  in
   tf

