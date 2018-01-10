mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

g x = if x > 'c' then 't' else 'f'

main = putStrLn $ mymap g "abcdefg"
