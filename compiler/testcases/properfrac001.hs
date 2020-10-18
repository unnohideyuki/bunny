properfrac :: (Integral b) => Float -> (b, Float)
properfrac x = let (n :% d) = toRational x
                   (q, r) = quotRem n d
                   b = fromInteger q
                   a = fromInteger r / fromInteger d
               in (b, a)

main = print $ properfrac (3.14::Float)
