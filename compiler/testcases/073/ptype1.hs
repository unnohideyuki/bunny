main = do print p
            where
              x = (1, 2)
              p = case x of
                (a, b) -> a


