f []       = False
f (x:[])   = False
f (x:y:[]) = x > y
f (x:xs)   = f xs

main = putStrLn $ show $ f "331223"
