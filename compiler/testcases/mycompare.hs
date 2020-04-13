mycompare x y | x == y    = EQ
              | x <= y    = LT
              | otherwise = GT

main = do
  print (mycompare 1 2)
  print (mycompare 100 2)
  print (mycompare 'a' 'a')


