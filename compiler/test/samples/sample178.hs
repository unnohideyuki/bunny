main =do  print $ take 5 [n..]
          print $ take 10 [n,(n-1)..]
          print [1..(m-1)]
          print [1,3..m]
            where n = 9223372036854775807 * 2
                  m = 8

