main = do print $ until (> 100) (*2) 1
          print $ until odd (`div` 2) 400
