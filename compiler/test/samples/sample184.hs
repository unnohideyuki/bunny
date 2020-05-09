x :: Double
x = 123456789012345678901234567890 :: Double

y :: Double
y = 3

main = do print (x + y)
          print (x - y)
          print (x * y)
          print (signum (1 :: Double))
          print (signum (0 :: Double))
          print (signum (-1 :: Double))

