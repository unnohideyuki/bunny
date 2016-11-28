foldr2 k z = go k z
  where
    go k z xs = case xs of
      [] -> z
      (y:ys) -> y `k` go k z ys 

s = foldr2 (++) "" ["a", "", "c", "d", "", "f"]

main = putStrLn s
