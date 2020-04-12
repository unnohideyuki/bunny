nodups []       = []
nodups [x]      = [x]
nodups (y:x:xs) = let
  f True  = nodups (x:xs)
  f False = y : nodups (x:xs)
  in
    f (x == y)

main = putStrLn $ nodups "331223"

