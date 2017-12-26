s True  = "True"
s False = "False"

main = do
  putStrLn $ s ((\x y -> x > y) 100 10)


