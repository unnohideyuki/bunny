f :: String -> Int
f x = case x of
  "zero" -> 0
  _      -> -1

main = do print $ f "hoge"
          print $ f "zero"
