-- qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]

-- main :: IO ()
main = 
  do let helo = "Hello, World!" 
     putStrLn helo
     putStrLn.show $ qsort [3, 1, 4, 1, 5, 9, 2, 6, 5]
     putStrLn $ show $ qsort helo
