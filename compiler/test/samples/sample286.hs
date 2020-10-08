f :: Double -> String
f 0.0 = "0.0"
f _   = "non-zero"
main = do putStrLn $ f 1.0
          putStrLn $ f 0.0
