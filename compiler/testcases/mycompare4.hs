mycompare x y = let
  f True  = EQ
  f False = let
    g True  = LT
    g False = GT
    in g (x <= y)
  in f (x == y)

main = do
  print (mycompare 1 2)
  print (mycompare 100 2)
  print (mycompare 'a' 'a')


