main = do putStrLn $ (\_ -> "PWildcard * 1") 0
          putStrLn $ (\a _ -> show a ++ ", PWildcard") 0 1
          putStrLn $ (\_ b -> "PWildcard, " ++ show b) 0 1
          putStrLn $ (\_ _ -> "PWildcard * 2") 0 1


