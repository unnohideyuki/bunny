move_left (x, y) = (x - 1, y)
move_up (x, y) = (x, y - 1)

main = do
  putStrLn $ show $ move_left (5, 5)
  putStrLn $ show $ move_up (5, 5)

