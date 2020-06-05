as = [True, True, True]
bs = [False, True, False]
cs = [False, False, False]

main = do print $ and as
          print $ or  as
          print $ and bs
          print $ or  bs
          print $ and cs
          print $ or  cs

