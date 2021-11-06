f x = case x of
  0 -> "zero"
  _ -> "non-zero"

main = do putStrLn $ f 1
          putStrLn $ f 0
          putStrLn $ f (1::Int)
          putStrLn $ f (0::Integer)
          putStrLn $ f (1.0::Double)
          putStrLn $ f (0.0::Float)

