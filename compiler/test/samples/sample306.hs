x = 1.0 :: Float
y = 2.0 :: Float
z = 0.0 :: Float
t = 10.0 :: Float

main = do print $ succ (2.3 :: Float)
          print $ pred (2.3 :: Float)
          print $ (toEnum 3 :: Float)
          print $ fromEnum (2.3 :: Float)
          print $ take 10 $ [x..]
          print $ take 10 $ [y, x ..]
          print $ [z .. t]
          print $ [z, y .. t]
