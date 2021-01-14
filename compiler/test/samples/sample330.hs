s = "[123456789012345678901234567890, 2, 3, 4, 5]"

xs :: [Integer]
xs = read s

main = print $ map (*2) xs
