f x y = let z = x * y
            c = x == y
        in (z, c)

g x y = print (x * y)

main = do print $ f 1 2
          print $ f (1::Double) 1
          g 3 3
          g (3::Double) 10
