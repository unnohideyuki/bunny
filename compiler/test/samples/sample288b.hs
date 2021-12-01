print' n x = putStrLn $ take n $ show x
  

main = do print (pi :: Double)
          print' 17 (exp 1 :: Double)
          print (log 10 :: Double)
          print (sqrt 2 :: Double)
          print (pi ** 2 :: Double)
          print (logBase 2 10 :: Double)
          print (sin 1 :: Double)
          print (cos 1 :: Double)
          print' 17 (tan 1 :: Double)
          print (asin (-1) :: Double)
          print (acos (-1) :: Double)
          print (atan 1 :: Double)
          print (sinh 1 :: Double)
          print' 16 (cosh 1 :: Double)
          print (tanh 1 :: Double)
          print' 16 (asinh 1 :: Double)
          print (acosh 2 :: Double)
          print' 17 (atanh 0.5 :: Double)

