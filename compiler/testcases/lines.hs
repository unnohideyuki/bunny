main = do print xs
          print $ unlines xs
            where xs = lines "abc\nd\nef\n"

