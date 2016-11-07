f (x:xs) = 'b' : f xs
-- f [] = []
f n = n

main = putStrLn (f "12345")
