type Foo a b = String -> (a, b)

foo :: Foo Int Char
foo s = (length s, head s)

main = print $ foo "Hello!"
