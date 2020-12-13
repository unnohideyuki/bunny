xs = [Just 3, Just 2, Nothing, Just 9, Nothing, Nothing, Just 8]
main = print $ sum $ [y | Just x <- xs, let y = x * 2]
