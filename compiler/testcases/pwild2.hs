f b = case b of
  True -> False
  _    -> True

main = print $ f False
