main = f s
  where
    s =
      let
        s' = "Hello, nested let!"
      in
       s'

    f =
      let
        g = putStrLn
      in
       g
           