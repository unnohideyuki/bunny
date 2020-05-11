main = do
  print $ (divMod 10 3 :: (Int, Int))
  print $ (quotRem 10 3 :: (Int, Int))
  print $ (divMod 10 (-3) :: (Int, Int))
  print $ (quotRem 10 (-3) :: (Int, Int))
  print $ (divMod 8 3 :: (Int, Int))
  print $ (quotRem 8 3 :: (Int, Int))
  print $ (divMod (-8) 3 :: (Int, Int))
  print $ (quotRem (-8) 3 :: (Int, Int))
