main = do
  print $ LT == LT
  print $ LT == EQ
  print $ LT == GT
  print $ EQ == LT
  print $ EQ == EQ
  print $ EQ == GT
  print $ GT == LT
  print $ GT == EQ
  print $ GT == GT

  print $ LT /= LT
  print $ LT /= EQ
  print $ LT /= GT
  print $ EQ /= LT
  print $ EQ /= EQ
  print $ EQ /= GT
  print $ GT /= LT
  print $ GT /= EQ
  print $ GT /= GT
