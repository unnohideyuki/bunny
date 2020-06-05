main = do print $ any (1==) [0..5]
          print $ any (>10) [0..5]
          print $ all (<10) [1..9]
          print $ all (==1) [1..9]
