class MyShow a where
  myshow :: a -> [Char]

instance MyShow Int where
  myshow = show

instance MyShow Char where
  myshow = show

instance (MyShow a) => MyShow [a] where
  myshow []     = "[]"
  myshow (x:xs) = myshow x ++ ":" ++ myshow xs

myshow' []     = "[]"
myshow' (x:xs) = myshow x ++ ":" ++ myshow xs

main = do
  putStrLn $ myshow a
  putStrLn $ myshow "Hello!"
  where
    a :: [Int]
    a = [1, 2, 3, 4, 5]

