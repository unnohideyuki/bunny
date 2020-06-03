f :: Int -> Either Int [Char]
f 0 = Left 0
f 1 = Left 2
f 2 = Right "abc"
f _ = Right "abcd"

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
