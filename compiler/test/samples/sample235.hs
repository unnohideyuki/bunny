main = do print $ maybe 0 (*2) Nothing
          print $ maybe 0 (*2) (Just 5)
