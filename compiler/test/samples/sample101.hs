f x y = x > y

s True = "True"
s False = "False"

main = do
  putStrLn $ s (f 100 10)
  putStrLn $ s (f 'x' 'y')

