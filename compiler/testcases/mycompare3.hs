mycompare x y = if x == y
                then EQ
                else if x <= y
                     then LT
                     else GT

main = do
  print (mycompare 1 2)
  print (mycompare 100 2)
  print (mycompare 'a' 'a')



