main = do print xs
          putStrLn $ unlines xs
            where xs = lines "abc\nd\nef\n"

