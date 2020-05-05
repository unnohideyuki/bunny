main =do  print $ take 5 [n..]
          print $ take 10 [n,(n-1)..]
          print [1..(n-1)]
          print [1,3..n]
            where n :: Int
                  n = 13

