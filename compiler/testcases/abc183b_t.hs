getIntList :: IO [Int]
getIntList = do s <- getLine
                return $ (map read . words) s

main = do
  [sx, sy, gx, gy] <- getIntList
  print (fromIntegral (sx*gy + gx*sy) / fromIntegral (sy + gy) :: Double)

