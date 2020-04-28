data Pair a b = Pair a b

instance (Show a, Show b) => Show (Pair b a) where
  show (Pair x y) = "Pair " ++ show x ++ " " ++ show y

main = print (Pair 1 'x')



