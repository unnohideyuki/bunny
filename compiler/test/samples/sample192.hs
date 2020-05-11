main = do
  print $ (divMod 10 3 :: (Integer, Integer))
  print $ (quotRem 10 3 :: (Integer, Integer))
  print $ (divMod 10 (-3) :: (Integer, Integer))
  print $ (quotRem 10 (-3) :: (Integer, Integer))
  print $ (divMod 8 3 :: (Integer, Integer))
  print $ (quotRem 8 3 :: (Integer, Integer))
  print $ (divMod (-8) 3 :: (Integer, Integer))
  print $ (quotRem (-8) 3 :: (Integer, Integer))

