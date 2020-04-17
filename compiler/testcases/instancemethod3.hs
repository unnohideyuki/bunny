class MyShow a where
  myshow :: a -> [Char]

data A = A1 | A2

instance MyShow A where
  myshow x = case x of
    A1 -> "A1"
    A2 -> "A2"

instance (MyShow a) => MyShow [a] where
  myshow x = "[" ++ showls x ++ "]"
    where showls x = case x of
                       []     -> ""
                       (x:[]) -> myshow x
                       (x:xs) -> myshow x ++ "," ++ showls xs


print' x = putStrLn $ myshow x

main = do print' A1
          print' [A1, A2, A2, A1]


