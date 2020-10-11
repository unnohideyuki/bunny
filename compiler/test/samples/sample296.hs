main = do print $ zipWith3 (\x y z -> x + 2*y + 3*z) [1..5] [5..10] [10..15]
          print $ zip3 [1..5] [9,8..7] [1,1,1,1,1]
          print $ unzip3 [(1,2,3), (2,3,4), (3,4,5)]
