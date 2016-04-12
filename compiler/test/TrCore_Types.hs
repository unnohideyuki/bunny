module TrCore_Types where

import Typing as Ty
import Types as Ty
import TrCore

import Test.Hspec

-- as1 -- Sample [Assump] that is similar to that of testcases/sample1.hs
as1 :: [Ty.Assump]
as1 = let
  t = Ty.TAp
      (Ty.TCon (Ty.Tycon
                "Main.IO"
                (Ty.Kfun Ty.Star Ty.Star)))
      (Ty.TCon (Ty.Tycon "()" Ty.Star))
  qt = [] Ty.:=> t
  sc = Ty.Forall [] qt
  in ["Main.main" Ty.:>: sc]

               
      

              
              
      
