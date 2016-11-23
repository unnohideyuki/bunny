f s = [s | c <- s, c <= 'd']

main = putStrLn $ f "abcdefgh"