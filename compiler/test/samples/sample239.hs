main = do s1 <- return "foo"
          s2 <- return "bar"
          putStrLn $ unwords [s1, s2]

