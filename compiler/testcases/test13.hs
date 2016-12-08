isNine x = (x == 9)
isNine' x = (x + 1 == 10)

main = do
  putStrLn $ show $ isNine 8
  putStrLn $ show $ isNine 9
  putStrLn $ show $ isNine' 8
  putStrLn $ show $ isNine' 9
