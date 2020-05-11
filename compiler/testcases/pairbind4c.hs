main = do print p
          print r
          print x
            where
              x :: (Int, Int)
              x = (1, 2)
              p = case x of
                    (a, b) -> a
              r = case x of
                    (a, b) -> b

