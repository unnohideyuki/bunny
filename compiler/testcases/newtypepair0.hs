data Pair b a = Pair (a, b) deriving Show

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

main = print $ fmap (*100) (Pair (2, 3))
