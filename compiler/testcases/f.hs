f []     = 0
f (x:xs) = 1 + f xs

main = putStrLn $ show $ f "abcde"
