init'        :: [a] -> [a]
init' [x]    =  []
init' (x:xs) =  x : init' xs

main = putStrLn $ init' "abcdef"
