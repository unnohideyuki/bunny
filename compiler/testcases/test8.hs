f x y = case x > y of
  True -> "True"
  False -> "False"

main = do
  putStrLn (f 100 10)
  putStrLn (f 'b' 'a')
