nodups xs = let
  g []       _       = []
  g [x]      _       = [x]
  g (y:x:xs) nodups' = let
    f True  nodups'' = nodups'' (x:xs)
    f False nodups'' = y : nodups'' (x:xs)
    in
      f (x == y) nodups'
  in g xs nodups

main = putStrLn $ nodups "331223"

