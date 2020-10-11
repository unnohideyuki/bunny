a = 42 :: Int
b = 5 :: Integer
c = 3.14 :: Float
d = pi :: Double

main = do print $ (realToFrac a :: Float)
          print $ (realToFrac b :: Float)
          print $ (realToFrac c :: Float)
          print $ (realToFrac d :: Float)
          print $ (realToFrac a :: Double)
          print $ (realToFrac b :: Double)
          print $ (realToFrac c :: Double)
          print $ (realToFrac d :: Double)

