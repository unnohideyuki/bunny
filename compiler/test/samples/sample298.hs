main = do print $ fmap (*2) (Just 5)
          print $ fmap (*2) Nothing
          print $ fmap (*2) [1, 2, 3]
