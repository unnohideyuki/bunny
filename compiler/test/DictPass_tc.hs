module DictPass_tc where

import Core
import DictPass

import Test.Hspec

-- core language sample

-- tstr -- String :: Type
tstr = TyConApp
       (TyCon "[]" (Kfun Star Star))
       [TyConApp (TyCon "Char" Star) []]

-- tiounit -- IO () :: type
tiounit = TyConApp
          (TyCon "Main.IO" (Kfun Star Star))
          [TyConApp (TyCon "()" Star) []]

-- es1 -- "Hello!" :: Expr
es1 = Lit $ LitStr "Hello!" tstr

-- es2 --  "World!" :: Expr
es2 = Lit $ LitStr "World!" tstr

-- eputStrLn -- putStrLn :: Expr
eputStrLn = Var (TermVar "Prim.putStrLn" (FunTy tstr tiounit))

-- e1 -- putStrLn "Hello!" :: Expr
e1 = App eputStrLn es1

-- e2 -- putStrLn "World!" :: Expr
e2 = App eputStrLn es2

-- ebind -- (>>) :: Expr
tbind = (FunTy
         (DictTy "$Main.Monad" "a0")
         (FunTy
          (AppTy
           (TyVarTy (TypeVar "a0" (Kfun Star Star)))
           (TyVarTy (TypeVar "a1" Star)))
          (FunTy
           (AppTy
            (TyVarTy (TypeVar "a0" (Kfun Star Star)))
            (TyVarTy (TypeVar "a2" Star)))
           (AppTy
            (TyVarTy (TypeVar "a0" (Kfun Star Star)))
            (TyVarTy (TypeVar "a2" Star))))))
ebind = Var (TermVar "Main.>>" tbind)

-- emain
e3 = (App ebind e1)
emain = App e3 e2

main = do
  -- print emain
  print $ getTy e3
  let (ds, t) = extrDictTy tbind
  print ds
  print t
  print $ map tyVars ds
  print $ tyVars t
