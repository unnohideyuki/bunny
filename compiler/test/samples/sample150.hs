qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]


myShowList xs = foldr (\x s -> show x ++ " " ++ s) "" xs

main :: IO ()
main =
  do let helo = "Hello, World!"
     -- putStrLn helo
     putStrLn.myShowList $ qsort [3, 1, 4, 1, 5, 9, 2, 6, 5]
     putStrLn $ myShowList $ qsort helo
