g k = f "hoge"
  where
    f [] = []
    f (x:xs) = 'a' `k` f xs
