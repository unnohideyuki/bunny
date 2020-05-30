main = do print $ sequence [Just "hello", Just "world"]
          print $ sequence [Just "hello", Nothing, Just "world"]
