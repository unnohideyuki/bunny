nodups [] = []
nodups [x] = [x]
nodups (y:x:xs) | y == x    = nodups (x:xs)
                | otherwise = y : nodups (x:xs)

main = putStrLn $ nodups "331223"
