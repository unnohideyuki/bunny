c [] ys = putStrLn ys
c (x:xs) ys = do putStrLn [x]
                 c xs ys

main = c "hello, " "world"
