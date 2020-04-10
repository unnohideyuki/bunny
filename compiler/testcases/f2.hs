f xs = case xs of
  []     -> 0
  (x:xs) -> 1 + f xs

main = putStrLn $ show $ f "abcde"
