module TrCore where -- Translate from Pattern.Expr to Core.

import Symbol
import Core
import qualified Pattern as Pat
import qualified Typing as Ty
import qualified Types as Ty

import Debug.Trace

trKind :: Ty.Kind -> Kind
trKind Ty.Star = Star
trKind (Ty.Kfun k1 k2) = Kfun (trKind k1) (trKind k2)

trType :: Ty.Type -> Type
trType (Ty.TCon (Ty.Tycon n k)) = TyConApp (TyCon n (trKind k)) []
trType (Ty.TAp t1 t2) =
  let
    t1' = trType t1
    t2' = trType t2
  in
   case t1' of
     TyConApp tycon ts -> TyConApp tycon (ts ++ [t2'])
     -- TODO: FynTy
     _                 -> AppTy t1' t2'

trType _ = TyVarTy (TypeVar "a" Star) -- Dummy value that make the test fail.
