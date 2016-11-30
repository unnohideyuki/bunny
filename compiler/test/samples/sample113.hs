f s = [c | c <- s, c <= 'd']

main = putStrLn $ f "abcdefgh"