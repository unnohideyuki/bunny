f s x = [c | c <- s, c <= x]

main = putStrLn $ f "abcdefgh" 'e'
