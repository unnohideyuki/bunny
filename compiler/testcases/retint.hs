strlen ""     = 0
strlen (c:cs) = 1 + strlen cs

main = return (strlen "Haskell")
