main = do
  putStrLn $ showList [1] ""
    where showlist' [] = "[]"
          showlist' [x] =  "[" ++ show x ++ "]"
          showlist' (x:xs) = "[" ++ foldl (\s t -> s ++ "," ++ show t) (show x) xs ++ "]"

          showList xs = (++) (showlist' xs)
