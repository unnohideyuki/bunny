pseq :: a -> b -> b
pseq x y = y

main = putStrLn (undefined `pseq` "abcd")
