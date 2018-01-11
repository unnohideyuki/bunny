f []     = False
f (x:xs) = x > '0'

main = putStrLn $ show $ f "331223"
