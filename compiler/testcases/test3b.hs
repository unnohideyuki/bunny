module Main where

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
  return = primRetIO
  (>>=)  = primBindIO
  fail s = primFailIO s

main = do
  putStrLn "Hello!"
  putStrLn "World!"

