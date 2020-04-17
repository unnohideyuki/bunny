class MyShow a where
  myshow :: a -> [Char]

data A = A1 | A2
data B a = B a

instance MyShow A where
  myshow A1 = "A1"
  myshow A2 = "A2"

instance (MyShow a) => MyShow (B a) where
  myshow (B a) = "B " ++ myshow a

print' x = putStrLn $ myshow x

main = do print' A1
          print' (B A1)
          print' (B (B A2))

