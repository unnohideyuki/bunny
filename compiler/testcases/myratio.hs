infixl 7 %

ratPrec = 7 :: Int

-- todo: (Integral a) =>
--       :%, infix constructor
--       deriving Eq
data (Integral a) => Ratio a = a :% a

type Rational = Ratio Integer

instance (Eq a) => Eq (Ratio a) where
  a :% b == c :% d = a == c && b == d

reduce _ 0 = error "Ratio.% : zero denominator"
reduce x y = (x `quot` d) :% (y `quot` d)
  where d = gcd x y

x % y      = reduce (x * signum y) (abs y)

numerator   (x :% _) = x
denominator (_ :% y) = y

instance (Integral a) => Ord (Ratio a) where
  x :% y <= x' :% y' = x * y' <= x' * y
  x :% y <  x' :% y' = x * y' <  x' * y

instance (Integral a) => Num (Ratio a) where
  x :% y + x' :% y' = reduce (x*y' + x'*y) (y*y')
  x :% y * x' :% y' = reduce (x*x') (y*y')
  negate (x :% y)   = (-x) :% y
  abs (x :% y)      = (abs x) :% y
  signum (x :% y)   = (signum x) :% 1
  fromInteger x     = (fromInteger x) :% 1


-- todo: Integral is a instance of Show?
--       showParen
instance (Show a) => Show (Ratio a) where
  show (x :% y) = show x ++ " % " ++ show y

main = do print a
          print b
          -- print c
          print (a < b)
          print $ a + b
          print $ a * b
          print (- a)
          print (fromInteger 8 :: Ratio Int)
  where a, b :: (Ratio Integer)
        a = 2 % 6
        b = 768 % (768 * 2)
        -- c = 'a' :% 'c' -- should be an error

