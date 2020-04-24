class MyShow a where
  myshow :: a -> [Char]

instance MyShow Int where
  myshow = show

instance MyShow Char where
  myshow = show

instance (MyShow a, MyShow b, MyShow c) => MyShow (a, b, c) where
  myshow (a, b, c) = "(" ++ myshow a ++ ", " ++ myshow b ++ ", " ++ myshow c ++ ")"

myshow' (a, b, c) = "(" ++ myshow a ++ ", " ++ myshow b ++ ", " ++ myshow c ++ ")"


main = do
  putStrLn $ myshow a
  putStrLn $ myshow ('+', '-', '*')
  where
    a :: (Int, Int, Char)
    a = (3, 1, 'z')

