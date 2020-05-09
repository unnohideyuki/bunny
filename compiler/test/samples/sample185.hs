x :: Float
x = 123456789012345678901234567890 :: Float

y :: Float
y = 3

main = do print (x + y)
          print (x - y)
          print (x * y)
          print (signum (1 :: Float))
          print (signum (0 :: Float))
          print (signum (-1 :: Float))
