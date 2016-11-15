module Prelude where

infixr 5 :

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

infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f (g x)

infixr 5 ++
(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

concatMap  :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr ((++) . f) []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
            where go []     = z
                  go (y:ys) = y `k` go ys

infix 4 ==, /=, <, <=, >=, >

not :: Bool -> Bool
not True = False
not False = True

class Eq a where
  (==),(/=) :: a -> a -> Bool
  -- Minimal Complete Definition:
  -- (==) or (/=)
  x /= y = not (x == y)
  x == y = not (x /= y)

class (Eq a) => Ord a where
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  (>) :: a -> a -> Bool

instance Ord Char where
  (<)  = Prim.charLt
  (<=) = Prim.charLe
  (>=) = Prim.charGe
  (>)  = Prim.charGt

instance Eq Char where
  (==) = Prim.charEq

class (Eq a) => Num a where
  (+) :: a -> a -> a

instance Num Integer where
  (+)  = Prim.integerEq -- not Eq!

instance Ord Integer where
  (<)  = Prim.integerLt
  (<=) = Prim.integerLe
  (>=) = Prim.integerGe
  (>)  = Prim.integerGt
