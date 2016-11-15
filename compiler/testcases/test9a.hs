-- testing the fixty resolution of infixr :, ++

infixr 5 `c`

c [] ys = ys
c (x:xs) ys = x : xs `c` ys

main = putStrLn $ "hello, " `c` "world"

