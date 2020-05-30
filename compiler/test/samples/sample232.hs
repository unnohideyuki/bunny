meq :: (Eq t) => [t] -> [t] -> Bool
[]  `meq` []  = True
[x] `meq` [y] = x == y
_   `meq` _   = False

main = do print $ [3] `meq` [2]
          print $ [5] `meq` [5]
