s = "[1, 2, 3, 4, 5]"

xs :: [Int]
xs = read s

main = print $ map (*2) xs
