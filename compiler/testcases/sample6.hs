main = x
  where
    x =
      let
        f = putStrLn
        s = "Hello, free variables!"
      in
       f s
