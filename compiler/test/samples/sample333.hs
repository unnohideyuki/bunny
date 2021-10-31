x :: Double
x = pi

y :: Float
y = pi

main = do print $ floatRadix x
          print $ floatRadix y
          print (encodeFloat 2 6 :: Double)
          print (encodeFloat 2 6 :: Float)
          print (encodeFloat 4 (-4) :: Double)
          print (encodeFloat 4 (-4) :: Float)
          print (encodeFloat (-2) (-4) :: Double)
          print (encodeFloat (-2) (-4) :: Float)
          print (acos 2 :: Double)
          print (acos 2 :: Float)
          print $ isNaN x
          print $ isNaN y
          print $ isNaN (acos 2 :: Double)
          print $ isNaN (acos 2 :: Float)
          print $ isInfinite x
          print $ isInfinite y
          print $ isInfinite (x / 0)
          print $ isInfinite (y / 0)
          print $ isDenormalized x
          print $ isDenormalized y
          print $ isDenormalized $ (encodeFloat 1 (-127) :: Float)
          print $ isDenormalized $ (encodeFloat 1 (-1023) :: Double)
          print $ isNegativeZero (0.0 :: Float)
          print $ isNegativeZero (0.0 :: Double)
          print $ isNegativeZero ((-x) / (1 / 0))
          print $ isNegativeZero ((-y) / (1 / 0))
          print $ isIEEE x
          print $ isIEEE y
          print $ exponent x
          print $ exponent y
          print $ decodeFloat x
          print $ decodeFloat y
          print $ significand x
          print $ significand y
          print $ scaleFloat 1 x
          print $ scaleFloat 1 y
          print $ atan2 (1.0 :: Double) 1.0
          print $ atan2 (1.0 :: Float) 1.0


