main =do  print $ take 5 ([n..] :: [Int])
          print $ take 10 ([n,(n-1)..] :: [Int])
          print ([1..(n-1)] :: [Int])
          print ([1,3..n] :: [Int])
            where n = 13

