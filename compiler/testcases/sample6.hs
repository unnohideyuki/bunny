main = x
  where
    s = "Hello, free variables"
    x =
      let
        f = putStrLn
      in
       f s

      
