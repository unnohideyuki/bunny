f s1 s2 = putStrLn $ s1 ++ s2

main = do
  "foo" `f` "bar"
  putStrLn "Hello!"
