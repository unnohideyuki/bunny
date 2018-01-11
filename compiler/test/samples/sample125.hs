f []     = False
f (x:xs) = x > x

main = putStrLn $ show $ f "331223"
