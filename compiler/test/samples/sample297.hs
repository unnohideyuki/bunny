x = 314 / 100 :: Double
y = 314 / 100 :: Float
z = (-5 / 1000) :: Double

main = do print $ toRational x
          print $ toRational y
          print $ toRational z
          print $ toRational (1/0 :: Double)
          print $ toRational (-1/0 :: Double)
          print $ toRational (0/0 :: Double)

