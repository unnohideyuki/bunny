f 0.0 = "0.0"
f _   = "non-zero"
main = do putStrLn $ f 1
          putStrLn $ f 0.0
