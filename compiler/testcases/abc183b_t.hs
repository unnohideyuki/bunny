getIntList :: IO [Int]
getIntList = return [1, 1, 7, 2]

main = do
  [sx, sy, gx, gy] <- getIntList
  print (fromIntegral (sx*gy + gx*sy) / fromIntegral (sy + gy) :: Double)

