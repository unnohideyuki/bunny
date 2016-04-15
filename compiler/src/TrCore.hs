module TrCore where -- Translate from Pattern.Expr to Core.

import Symbol
import Core
import qualified Pattern as Pat
import qualified Typing as Ty
import qualified Types as Ty

import Debug.Trace

trType :: Ty.Type -> Type
trType _ = TyVarTy (TypeVar "a" Star) -- Dummy value that make the test fail.
