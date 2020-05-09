quotRem' n d = if signum r == - signum d then (q+1, r-d) else (q, r)
  where (q, r) = divMod n d

xys = [(10, 3), (8, 3), (7, 5), (100, 10)]
sigs = [(1, 1), (1, -1), (-1, 1), (-1, -1)]

check [] = return ()
check ((x, y):xys') = do loop x y sigs
                         check xys'
  where loop _ _ [] = return ()
        loop x y ((sx, sy): sigs') = do
          let x' = x * sx
              y' = y * sy
              (q, r) = quotRem x' y'
              (q', r') = quotRem x' y'
              res = (q, r) == (q', r')
          print (q, r, q', r', res)
          loop x y sigs'

main = check xys
