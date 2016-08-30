module DictPass_tc where

import Core
import DictPass
import Types
import Typing (Pred(..), Qual(..))

import Test.Hspec

-- core language sample

-- es1 -- "Hello!" :: Expr
es1 = Lit $ LitStr "Hello!" tString

-- es2 --  "World!" :: Expr
es2 = Lit $ LitStr "World!" tString

-- eputStrLn -- putStrLn :: Expr
tIO = (TCon (Tycon "Main.IO" (Kfun Star Star)))
tiounit = TAp tIO tUnit
eputStrLn = Var (TermVar "Prim.putStrLn" ([] :=> (tString `fn` tiounit)))

-- e1 -- putStrLn "Hello!" :: Expr
e1 = App eputStrLn es1

-- e2 -- putStrLn "World!" :: Expr
e2 = App eputStrLn es2

-- ebind -- (>>) :: Expr
tbind =  (fn
          (TAp (TGen 0) (TGen 1))
          (fn
           (TAp (TGen 0) (TGen 2))
           (TAp (TGen 0) (TGen 2))))

ebind = Var (TermVar "Main.>>" ([IsIn "Main.Monad" (TGen 0)] :=>tbind))

e3 = (App ebind e1)
emain1 = App e3 e2

-- e4
e4 = (App ebind (Var
                 (TermVar "return()"
                  ([IsIn "Main.Monad" (TGen 0)] :=> TAp (TGen 0) tUnit))))

main :: IO ()
main = do
  print $ getTy e4
  hspec $ do
    describe "testing getTy" $ do
      it "putStrLn \"Hello!\" :: IO ()" $ do
        getTy e1 `shouldBe`  ([] :=> TAp tIO tUnit)
      it "putStrLn \"World!\" :: IO ()" $ do
        getTy e2 `shouldBe`  ([] :=> TAp tIO tUnit)
      it "(>>) putStrLn \"Hello!\" :: IO b -> IO IO b" $ do
        getTy e3 `shouldBe` ([]:=>((TAp tIO (TGen 2)) `fn` (TAp tIO (TGen 2))))
      it "main1 :: IO ()" $ do
        getTy emain1 `shouldBe`  ([] :=> TAp tIO tUnit)
      it "(>>) return () :: [] => m b -> m b" $ do
        getTy e4 `shouldBe` ([] :=>
                             ((TAp (TGen 0) (TGen 2)) `fn` (TAp (TGen 0) (TGen 2))))

  
