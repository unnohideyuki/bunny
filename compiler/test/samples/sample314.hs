main = do print (maxBound :: Int)
          print (minBound :: Int)
          print (maxBound + 1 == (minBound :: Int))
          print (minBound - 1 == (maxBound :: Int))
