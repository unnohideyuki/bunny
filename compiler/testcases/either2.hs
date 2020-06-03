f :: Int -> Either Integer Double
f 0 = Left 0
f 1 = Left 2
f 2 = Right 10
f _ = Right 3

main = do
  print $ f 0
  print $ f 1
  print $ f 2
  print $ f 3
  print $ f 0 == f 0
  print $ f 0 <= f 1
  print $ f 0 <= f 2
  print $ f 2 <= f 2
  print $ f 2 <= f 3
  print $ either (fromInteger . (*2)) (+2) (f 1)
  print $ either (fromInteger . (*2)) (+2) (f 3)

