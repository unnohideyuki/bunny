data A = A1 | A2 | A3
data B = B1 | B2

class MyShow a where
  myshow :: a -> [Char]

instance MyShow A where
  myshow A1 = "A1"
  myshow A2 = "A2"
  myshow A3 = "A3"

instance MyShow B where
  myshow B1 = "B1"
  myshow B2 = "B2"

data Pair a b = Pair a b

instance (MyShow a, MyShow b) => MyShow (Pair a b) where
  myshow (Pair x y) = "Pair " ++ myshow x ++ " " ++ myshow y

main = putStrLn $ myshow (Pair A1 B2)



