f b = case b of
  True -> 1
  _    -> 2

main = print $ f False
