module Prelude where

data Bool = False | True

infixl 1 >>, >>=

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  fail   :: String -> m a

  -- Minimal complete definition: (>>=), return
  p >> q = p >>= \_ -> q
  fail s = error s

instance Monad IO where
  return = Prim.retIO
  (>>=)  = Prim.bindIO
  fail s = Prim.failIO s

myshow (True, True) = "#t"
myshow _            = "#f"

main = do
  putStrLn (myshow (False, False))
  putStrLn (myshow (False, True))
  putStrLn (myshow (True,  False))
  putStrLn (myshow (True,  True))

