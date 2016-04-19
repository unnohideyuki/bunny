module TrCore_Types where

import qualified Typing as Ty
import qualified Types as Ty
import Core
import TrCore

import Test.Hspec

-- as1 -- Sample [Assump] that is similar to that of testcases/sample1.hs
as1 :: [Ty.Assump]
as1 = let
  qt = [] Ty.:=> t1
  sc = Ty.Forall [] qt
  in ["Main.main" Ty.:>: sc]

t1 :: Ty.Type
t1 = Ty.TAp
     (Ty.TCon (Ty.Tycon
               "Main.IO"
               (Ty.Kfun Ty.Star Ty.Star)))
     (Ty.TCon (Ty.Tycon "()" Ty.Star))

t1e :: Type
t1e = TyConApp (TyCon "Main.IO" (Kfun Star Star)) [TyConApp (TyCon "()" Star) []]

-- t2 -- Sample type from testcases/sample4.hs

t2 = Ty.TAp
     (Ty.TAp
      (Ty.TCon (Ty.Tycon "(->)" (Ty.Kfun Ty.Star (Ty.Kfun Ty.Star Ty.Star))))
      (Ty.TCon (Ty.Tycon "Bool" Ty.Star)))
     (Ty.TAp (Ty.TCon (Ty.Tycon "[]" (Ty.Kfun Ty.Star Ty.Star))) (Ty.TCon (Ty.Tycon "Char" Ty.Star)))

t2e = FunTy (TyConApp (TyCon "Bool" Star) []) (TyConApp (TyCon "[]" (Kfun Star Star)) [TyConApp (TyCon "Char" Star) []])

-- t0 -- Sample Type

t0 :: Ty.Type
t0 = Ty.TAp
     (Ty.TAp
      (Ty.TCon (Ty.Tycon "A" (Ty.Kfun Ty.Star (Ty.Kfun Ty.Star Ty.Star))))
      (Ty.TCon (Ty.Tycon "B" Ty.Star)))
     (Ty.TCon (Ty.Tycon "C" Ty.Star))

t0e :: Type
t0e = TyConApp (TyCon "A" (Kfun Star (Kfun Star Star))) [ TyConApp (TyCon "B" Star) []
                                                        , TyConApp (TyCon "C" Star) []
                                                        ]

main :: IO ()
main = hspec $ do
  describe "TrCore.trType" $ do
    it "converts from t0::Typing.Type to t0e::Core.Type " $ do
      trType t0 `shouldBe` t0e
    it "converts from t1::Typing.Type to t1e::Core.Type " $ do
      trType t1 `shouldBe` t1e
    it "converts from t2::Typing.Type to t2e::Core.Type " $ do
      trType t2 `shouldBe` t2e

    it "finds the type of Main.main " $ do
      tyLookup "Main.main" as1 `shouldBe` t1e
