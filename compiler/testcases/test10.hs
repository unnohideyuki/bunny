main = do
  putStrLn (f 100 10)
  putStrLn (f 'a' 'b')
  where
    f x y = case x > y of
      True -> "True"
      False -> "False"
    