f x = case x of
  0.0 -> "zero"
  _   -> "non-zero"

main = do putStrLn $ f (1.0::Double)
          putStrLn $ f (0.0::Float)

