f 0.0 = "0.0"
f _   = "non-zero"
main = do putStrLn $ f (1::Double)
          putStrLn $ f (0::Double)
