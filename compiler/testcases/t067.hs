main = do
  putStrLn $ f 1 ""
    where
      f xs str = str ++ (show' xs)
        where show' x =  show x

