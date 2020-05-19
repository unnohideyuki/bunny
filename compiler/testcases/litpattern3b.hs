f x = case x of
  '0' -> "'0'"
  _   -> "non-zero"
main = do putStrLn $ f '1'
          putStrLn $ f '0'
