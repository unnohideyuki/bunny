myComp x y = (x == y)
addComp x y = (x + 1 == y)

main = do
  putStrLn $ show $ myComp 8 9
  putStrLn $ show $ myComp 9 9
  putStrLn $ show $ addComp 8 9
  putStrLn $ show $ addComp 9 9
