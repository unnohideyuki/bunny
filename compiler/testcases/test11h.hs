main = do
  putStrLn $ s True
  putStrLn $ s False
  where
    s True = "True"
    s False = "False"
