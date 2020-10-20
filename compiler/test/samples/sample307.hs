x = 1.0 :: Double
y = 2.0 :: Double
z = 0.0 :: Double
t = 10.0 :: Double

main = do print $ succ (2.3 :: Double)
          print $ pred (2.3 :: Double)
          print $ (toEnum 3 :: Double)
          print $ fromEnum (2.3 :: Double)
          print $ take 10 $ [x..]
          print $ take 10 $ [y, x ..]
          print $ [z .. t]
          print $ [z, y .. t]
