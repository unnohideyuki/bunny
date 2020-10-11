main = do print $ head [1, 2, 3]
          print $ tail [1, 2, 3]
          print $ foldr1 (+) [1, 2, 3, 4]
          print $ take 4 (repeat 'x')
          print $ replicate 3 5
