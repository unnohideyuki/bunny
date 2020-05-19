f x = case x of
  0 -> "zero"
  _ -> "non-zero"

main = do putStrLn $ f 1
          putStrLn $ f 0
