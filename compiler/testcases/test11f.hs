go k z []     = z
go k z (y:ys) = y `k` go k z ys

foldr2 k z = go k z

s = foldr2 (++) "" ["a", "", "c", "d", "", "f"]

main = putStrLn s

