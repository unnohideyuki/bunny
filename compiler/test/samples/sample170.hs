strlen :: [Char] -> Int
strlen ""     = 0
strlen (c:cs) = 1 + strlen cs

main = print $ strlen "Haskell"
