main = do print $ zip [1..] "abcde"
          print $ zipWith (+) [1,2,3] [3,2,1]
          print $ unzip [('a',2),('b',3),('c',4)]
