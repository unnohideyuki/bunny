class MyShow a where
  myshow :: a -> [Char]

instance MyShow Int where
  myshow = show

instance MyShow Char where
  myshow = show

instance (MyShow a, MyShow b) => MyShow (a, b) where
  myshow (a, b) = "(" ++ myshow a ++ ", " ++ myshow b ++ ")"

myshow' (a, b) = "(" ++ myshow a ++ ", " ++ myshow b ++ ")"


main = do
  putStrLn $ myshow a
  putStrLn $ myshow ('a', 'Z')
  where
    a :: (Int, Int)
    a = (3, 9)

