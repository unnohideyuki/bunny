x = toRational (314/100 :: Double)
y = toRational (10/3 :: Double)

a :: Double
a = fromRational x

b :: Float
b = fromRational x

c :: Double
c = fromRational y

d :: Float
d = fromRational y

main = do print a
          print b
          print c
          print d

