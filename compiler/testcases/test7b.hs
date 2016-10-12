g = f "hoge"
  where
    f [] = []
    f (x:xs) = 'a' : f xs
