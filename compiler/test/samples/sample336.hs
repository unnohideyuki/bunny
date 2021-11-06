f x = case x of
  'X' -> "X"
  _   -> "not X"

main = do putStrLn $ f 'X'
          putStrLn $ f 'Y'
