f g x y = do
  g (x + y)
  g (x * y)

main = f print 2 3
