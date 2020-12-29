data X a b c d e = X a b c d e
main = do
  let X a b c d e = X 1 2 3 4 5
  print a
  print b
  print c
  print d
  print e
