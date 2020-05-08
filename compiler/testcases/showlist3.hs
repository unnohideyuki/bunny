main = do
  putStrLn $ showList [1] ""
    where
      showList xs = (++) (showlist' xs)
        where showlist' [] = "[]"
              showlist' [x] =  "[" ++ show x ++ "]"
              showlist' (x:xs) = "[" ++ foldl (\s t -> s ++ "," ++ show t) (show x) xs ++ "]"
              showlist' :: [Integer] -> [Char]
