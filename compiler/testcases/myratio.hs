infixl 7 %

ratPrec = 7 :: Int

-- todo: (Integral a) =>
--       :%, infix constructor
--       deriving Eq
--       Ratio a a (not Ratio a b)
data Ratio a = Rat a a

type Rational = Ratio Integer

instance (Eq a) => Eq (Ratio a) where
  Rat a b == Rat c d = a == c && b == d

reduce _ 0 = error "Ratio.% : zero denominator"
reduce x y = Rat (x `quot` d) (y `quot` d)
  where d = gcd x y

x % y      = reduce (x * signum y) (abs y)

numerator   (Rat x _) = x
denominator (Rat _ y) = y

instance (Integral a) => Ord (Ratio a) where
  Rat x y <= Rat x' y' = x * y' <= x' * y
  Rat x y <  Rat x' y' = x * y' <  x' * y

instance (Integral a) => Num (Ratio a) where
  Rat x y + Rat x' y' = reduce (x*y' + x'*y) (y*y')
  Rat x y * Rat x' y' = reduce (x*x') (y*y')
  negate (Rat x y)    = Rat (-x) y
  abs (Rat x y)       = Rat (abs x) y
  signum (Rat x y)    = Rat (signum x) 1
  fromInteger x       = Rat (fromInteger x) 1


-- todo: Integral is a instance of Show?
--       showParen
instance (Show a) => Show (Ratio a) where
  show (Rat x y) = show x ++ " % " ++ show y

main = do print a
          print b
          print (a < b)
          -- print $ a + b
          -- print $ a * b
          -- print (- a)
          -- print (fromInteger 8 :: Ratio Int)
  where a, b :: (Ratio Integer)
        a = 5 % 10
        b = 1 % 2
