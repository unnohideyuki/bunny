data X = X Int Int

main = print (a, b)
  where x = X 5 2
        a = case x of
          X x y -> x
        b = case x of
          X x y -> y
