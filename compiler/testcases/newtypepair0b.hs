data Pair b a = Pair (a, b)

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

instance (Show a, Show b) => Show (Pair b a) where
  show (Pair (x, y)) = "Pair (" ++ show x ++ "," ++ show y ++ ")"

main = print $ fmap (*100) (Pair (2, 3))
