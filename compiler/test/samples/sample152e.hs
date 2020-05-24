main = do
  print $ compare LT LT
  print $ compare LT EQ
  print $ compare LT GT
  print $ compare EQ LT
  print $ compare EQ EQ
  print $ compare EQ GT
  print $ compare GT LT
  print $ compare GT EQ
  print $ compare GT GT
