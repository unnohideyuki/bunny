main = e
  where
    s =
      let
        s = "Hello, letrec!"
      in
       s

    f =
      let
        f = putStrLn
      in
       f

    e = f s

           
