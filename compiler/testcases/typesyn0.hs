type Foo a = String -> (a, String)

foo :: Foo Int
foo s = (length s, reverse s)

main = print $ foo "Hello!"
