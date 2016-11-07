f [] = []
f (x:xs) = 'a' : f xs

main = putStrLn (f "hoge")
