module Desugar_ptypes where

import Typing
import Types
import Desugar

import Test.Hspec

ft1 = TAp
      (TAp
       (TCon (Tycon "(->)" (Kfun Star (Kfun Star Star))))
       (TCon (Tycon "Bool" Star)))
      (TAp (TCon (Tycon "[]" (Kfun Star Star))) (TCon (Tycon "Char" Star)))

ft2 = TAp
      (TAp
       (TCon (Tycon "(->)" (Kfun Star (Kfun Star Star))))
       (TCon (Tycon "Bool" Star)))
      (TAp
       (TAp
        (TCon (Tycon "(->)" (Kfun Star (Kfun Star Star))))
        (TCon (Tycon "Bool" Star)))
       (TAp (TCon (Tycon "[]" (Kfun Star Star))) (TCon (Tycon "Char" Star))))

ft3 = TAp
      (TAp
       (TCon (Tycon "(->)" (Kfun Star (Kfun Star Star))))
       (TCon (Tycon "Bool" Star)))
      (TAp
       (TAp
        (TCon (Tycon "(->)" (Kfun Star (Kfun Star Star))))
        (TCon (Tycon "Bool" Star)))
       (TAp
        (TAp
         (TCon (Tycon "(->)" (Kfun Star (Kfun Star Star))))
         (TCon (Tycon "Bool" Star)))
        (TAp (TCon (Tycon "[]" (Kfun Star Star))) (TCon (Tycon "Char" Star)))))

main :: IO ()
main = hspec $ do
  describe "Desugar.ptypes" $ do
    it "extract [Bool] from Bool -> [Char]" $ do
      ptypes ft1 `shouldBe` [(TCon (Tycon "Bool" Star))]
    it "extract [Bool, Bool] from Bool -> Bool -> [Char]" $ do
      ptypes ft2 `shouldBe` [(TCon (Tycon "Bool" Star))
                            ,(TCon (Tycon "Bool" Star))
                            ]
    it "extract [Bool, Bool,Bool] from Bool-> Bool -> Bool -> [Char]" $ do
      ptypes ft3 `shouldBe` [(TCon (Tycon "Bool" Star))
                            ,(TCon (Tycon "Bool" Star))
                            ,(TCon (Tycon "Bool" Star))
                            ]


