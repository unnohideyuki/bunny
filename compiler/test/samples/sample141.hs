nodups [] = []
nodups [x] = [x]
nodups (y:x:xs) = if y == x
                  then nodups (x:xs)
                  else y : nodups (x:xs)

main = putStrLn $ nodups "331223"
