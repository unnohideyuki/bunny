main = do
  print $ LT < LT
  print $ LT < EQ
  print $ LT < GT
  print $ EQ < LT
  print $ EQ < EQ
  print $ EQ < GT
  print $ GT < LT
  print $ GT < EQ
  print $ GT < GT

  print $ LT <= LT
  print $ LT <= EQ
  print $ LT <= GT
  print $ EQ <= LT
  print $ EQ <= EQ
  print $ EQ <= GT
  print $ GT <= LT
  print $ GT <= EQ
  print $ GT <= GT

  print $ LT >= LT
  print $ LT >= EQ
  print $ LT >= GT
  print $ EQ >= LT
  print $ EQ >= EQ
  print $ EQ >= GT
  print $ GT >= LT
  print $ GT >= EQ
  print $ GT >= GT

  print $ LT > LT
  print $ LT > EQ
  print $ LT > GT
  print $ EQ > LT
  print $ EQ > EQ
  print $ EQ > GT
  print $ GT > LT
  print $ GT > EQ
  print $ GT > GT

  print $ compare LT LT
  print $ compare LT EQ
  print $ compare LT GT
  print $ compare EQ LT
  print $ compare EQ EQ
  print $ compare EQ GT
  print $ compare GT LT
  print $ compare GT EQ
  print $ compare GT GT

  print $ max LT LT
  print $ max LT EQ
  print $ max LT GT
  print $ max EQ LT
  print $ max EQ EQ
  print $ max EQ GT
  print $ max GT LT
  print $ max GT EQ
  print $ max GT GT

{-
  print $ min LT LT
  print $ min LT EQ
  print $ min LT GT
  print $ min EQ LT
  print $ min EQ EQ
  print $ min EQ GT
  print $ min GT LT
  print $ min GT EQ
  print $ min GT GT
-}
