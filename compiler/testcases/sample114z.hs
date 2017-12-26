f a b = if a > b then [a] else [b]

main = putStrLn $ f 'b' 'a'
