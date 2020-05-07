take' :: Int -> [a] -> [a]
take' n xs     | n <= 0 = []
take' n []     = []
take' n (x:xs) = x : take' (n-1) xs

main = print $ take 5 [1, 2, 3, 4, 5, 6, 7]
