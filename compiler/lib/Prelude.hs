module Prelude where

infixr 9 .
infixr 8 ^, ^^, **
infixl 7 *, /, `quot`, `rem`, `div`, `mod`
infixl 6 +, -

infixr 5 :
infix  4 ==, /=, <, <=, >=, >
infixr 3 &&
infixr 2 ||
infixl 1 >>, >>=
infixr 1 =<<
infixr 0 $, $!, `seq`

-- Standard types, classes, instances and related functions

-- Equality and Ordered classes
  
class Eq a where
  (==), (/=) :: a -> a -> Bool
  -- Minimal Complete Definition:
  -- (==) or (/=)
  x /= y = not (x == y)
  x == y = not (x /= y)

instance (Eq a) => Eq [a] where
  [] == [] = True
  _  == [] = False
  [] == _  = False
  (x:xs) == (y:ys) = x == y && xs == ys

instance (Ord a) => Ord [a] where
  [] <= _  = True
  _  <= [] = False
  (x:xs) <= (y:ys) | x == y    = xs <= ys
                   | x <  y    = True
                   | otherwise = False

class (Eq a) => Ord a where
  compare              :: a -> a -> Ordering
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min             :: a -> a -> a
  -- Minimal complete definition:
  --   (<=) or compare
  compare x y
    | x == y    = EQ
    | x <= y    = LT
    | otherwise = GT
  x <= y = compare x y /= GT
  x <  y = compare x y == LT
  x >= y = compare x y /= LT
  x >  y = compare x y == GT
  max x y
    | x <= y    = y
    | otherwise = x
  min x y
    | x <= y    = x
    | otherwise = y

-- Enumeration and Bounded classes

class Enum a where
  succ, pred     :: a -> a
  toEnum         :: Int -> a
  fromEnum       :: a -> Int
  enumFrom       :: a -> [a]           -- [n..]
  enumFromThen   :: a -> a -> [a]      -- [n,n',..]
  enumFromTo     :: a -> a -> [a]      -- [n..m]
  enumFromThenTo :: a -> a -> a -> [a] -- [n,n'..m]
  -- Minimal complete definition: toEnum, fromEnum
  succ             = toEnum . (+1) . fromEnum
  pred             = toEnum . (subtract 1) . fromEnum
  enumFrom x       = map toEnum [fromEnum x ..]
  enumFromTo x y   = map toEnum [fromEnum x .. fromEnum y]
  enumFromThen x y = map toEnum [fromEnum x, fromEnum y ..]
  enumFromThenTo x y z
                   = map toEnum [fromEnum x, fromEnum y .. fromEnum z]

class Bounded a where
  minBound :: a
  maxBound :: a

type String = [Char]

---

instance Enum () where
  toEnum x | x == 0 = ()
           | otherwise = error "().toEnum: bad argument"
  fromEnum () = 0
  enumFrom () = [()]
  enumFrom _  = error "().enumFrom: bad argument"
  enumFromThen () () = repeat ()
  enumFromThen _  _  = error "().enumFromThen: bad argument"

instance Bounded () where
  minBound = ()
  maxBound = ()

-- Numeric classes

class (Eq a, Show a) => Num a where
  (+), (-), (*) :: a -> a -> a
  negate        :: a -> a
  abs, signum   :: a -> a
  fromInteger   :: Integer -> a
  -- Minimal complete definition:
  --  All, except negate or (-)
  x - y    = x + negate y
  negate x = 0 - x

class (Num a, Ord a, Enum a) => Integral a where
  quot, rem       :: a -> a -> a
  div, mod        :: a -> a -> a
  quotRem, divMod :: a -> a -> (a, a)
  toInteger       :: a -> Integer
  -- Minimal complete definition: quotRem, toInteger
  n `quot` d = q where (q,r) = quotRem n d
  n `rem`  d = r where (q,r) = quotRem n d
  n `div`  d = q where (q,r) = divMod n d
  n `mod`  d = r where (q,r) = divMod n d
  divMod n d = if signum r == - signum d then (q-1, r+d) else qr
    where qr@(q,r) = quotRem n d

-- (from here) todo: should be separated into Data.Ratio 

-- todo: (Integral a) =>
--       :%, infix constructor
--       deriving Eq
data (Integral a) => Ratio a = a :% a

type Rational = Ratio Integer

instance (Eq a) => Eq (Ratio a) where
  a :% b == c :% d = a == c && b == d

-- todo: Integral is a instance of Show?
--       showParen
instance (Integral a) => Show (Ratio a) where
  show (x :% y) | x < 0     = "(" ++ show x ++ ") % " ++ show y
                | otherwise = show x ++ " % " ++ show y

-- (to here) todo: should be separated into Data.Ratio 

class (Num a, Ord a) => Real a where
  toRational :: a -> Rational

class (Num a) => Fractional a where
  (/)   :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  -- Minimal complete definition:
  --     fromRational and (recip or (/))
  recip x = 1 / x
  x / y   = x * recip y

class (Fractional a) => Floating a where
  pi                  :: a
  exp, log, sqrt      :: a -> a
  (**), logBase       :: a -> a -> a
  sin, cos, tan       :: a -> a
  asin, acos, atan    :: a -> a
  sinh, cosh, tanh    :: a -> a
  asinh, acosh, atanh :: a -> a
  -- Minimal complete definition:
  --   pi, exp, log, sin, cos, sinh, cosh,
  --   asin, acos, atan,
  --   asinh, acosh, atanh
  x ** y      = exp (log x * y)
  logBase x y = log y / log x
  sqrt x      = x ** 0.5
  tan x       = sin x / cos x
  tanh x      = sinh x / cosh x

class (Real a, Fractional a) => RealFrac a where
  properFraction  :: (Integral b) => a -> (b, a)
  truncate, round :: (Integral b) => a -> b
  ceiling, floor  ::  (Integral b) => a -> b
  -- Minimal complete definition:  properFraction
  truncate x = m where (m, _) = properFraction x
  round x = let (n, r) = properFraction x
                m      = if r < 0 then n - 1 else n + 1
                s = signum (abs r - 0.5)
            in if (s == -1) || (s == 0 && even n)
               then n
               else m

  ceiling x = if r > 0 then n + 1 else n
    where (n, r) = properFraction x
  floor x = if r < 0 then n - 1 else n
    where (n, r) = properFraction x

class (RealFrac a, Floating a) => RealFloat a where
  floatRadix  :: a -> Integer
  floatDigits :: a -> Int
  floatRange  :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent    :: a -> Int
  significand :: a -> a
  scaleFloat  :: Int -> a -> a
  isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
              :: a -> Bool
  atan2       :: a -> a -> a
  -- Minimal complete definition:
  -- All except exponent, significand, scaleFloat, atan2
  exponent x = if m == 0 then 0 else n + floatDigits x
    where (m, n) = decodeFloat x
  significand x = encodeFloat m (- floatDigits x)
    where (m, _) = decodeFloat x
  scaleFloat k x = encodeFloat m (n+k)
    where (m, n) = decodeFloat x
  atan2 y x
    | x > 0            = atan (y/x)
    | x == 0 && y > 0  = pi / 2
    | x < 0  && y > 0  = pi + atan (y/x)
    | (x <= 0 && y < 0) ||
      (x < 0 && isNegativeZero y) ||
      (isNegativeZero x && isNegativeZero y)
                       = -atan2 (-y) x
    | y == 0 && (x < 0 || isNegativeZero y)
                       = pi -- must be after the previous test on zero y
    | x == 0 && y == 0 = y -- must be after the other double zero tests
    | otherwise        = x + y -- x or y is a NaN, return a NaN (via +)
  
-- Numeric functions

subtract :: (Num a) => a -> a -> a
subtract =  flip (-)

even, odd :: (Integral a) => a -> Bool
even n    =  n `rem` 2 == 0
odd       = not . even


gcd     :: (Integral a) => a -> a -> a
gcd 0 0 =  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y =  gcd' (abs x) (abs y)
           where gcd' x 0 = x
                 gcd' x y = gcd' y (x `rem` y)

lcm     :: (Integral a) => a -> a -> a
lcm _ 0 =  0
lcm 0 _ =  0
lcm x y =  abs ((x `quot` (gcd x y)) * y)


(^)           :: (Num a, Integral b) => a -> b -> a
x ^ 0         =  1
x ^ n | n > 0 =  f x (n-1) x
                 where f _ 0 y = y
                       f x n y = g x n where
                         g x n | even n = g (x*x) (n `quot` 2)
                               | otherwise = f x (n-1) (x*y)
_ ^ _         =  error "Prelude.^: negative exponent"

(^^)   :: (Fractional a, Integral b) => a -> b -> a
x ^^ n =  if n >= 0 then x^n else recip (x^(-n))

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral =  fromInteger . toInteger

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac =  fromRational . toRational

-- realToFrac

-- Monadic classes

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  return :: a -> m a
  fail   :: String -> m a
  -- Minimal complete definition: (>>=), return
  m >> k = m >>= \_ -> k
  fail s = error s

sequence :: Monad m => [m a] -> m [a]
sequence =  foldr mcons (return [])
  where mcons p q = p >>= \x -> q >>= \y -> return (x:y)

sequence_ :: Monad m => [m a] -> m ()
sequence_ =  foldr (>>) (return ())

-- The xxxM function take list arguments, but lift the function or
-- list element to a monad type
mapM      :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as =  sequence (map f as)

mapM_      :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as =  sequence_ (map f as)

(=<<)   :: Monad m => (a -> m b) -> m a -> m b
f =<< x =  x >>= f

-- Function type

-- identity function
id   :: a -> a
id x = x

-- constant function
const     :: a -> b -> a
const x _ =  x

-- function composition
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

-- flip f takes its (first) two arguments in the reverse order of f.
flip       :: (a -> b -> c) -> b -> a -> c
flip f x y =  f y x

seq :: a -> b -> b
seq = Prim.seq

-- right-associating infix application operators
-- (useful in continuation-passing style)
($), ($!) :: (a -> b) -> a -> b
f $ x = f x
f $! x = x `seq` f x

-- Boolean type

data Bool = False | True deriving (Eq, Show)

instance Ord Bool where
  False <= False = True
  False <= True  = True
  _     <= _     = False

instance Enum Bool where
  toEnum x | x == 0 = False
           | x == 1 = True
           | otherwise = error "Bool.toEnum: bad argument"
  fromEnum False = 0
  fromEnum True  = 1
  enumFrom b = map toEnum [fromEnum b .. 1]
  enumFromThen b b' = map toEnum [fromEnum b, fromEnum b' .. lastInt]
    where lastInt | b' < b = 0
                  | otherwise = 1

instance Bounded Bool where
  minBound = False
  maxBound = True

-- Boolean functions

(&&), (||) :: Bool -> Bool -> Bool
True  && x = x
False && x = False
True  || _ = True
False || x = x

not :: Bool -> Bool
not True  = False
not False = True

otherwise :: Bool
otherwise =  True

-- Character type

instance Eq Char where
  (==) = Prim.charEq

instance Ord Char where
  (<=) = Prim.charLe

instance Enum Char where
  toEnum = Prim.intToChar
  fromEnum = Prim.charToInt
  enumFrom c = map toEnum [fromEnum c .. 1114111]
  enumFromThen c c' = map toEnum [fromEnum c, fromEnum c' .. lastInt]
    where lastInt | c' < c    = 0
                  | otherwise = 1114111

instance Show Char where
  -- todo: escape
  show c = ['\'', c, '\'']
  showList cs = (:) '"' . showl cs
    where showl "" = (:) '"'
          showl ('"':cs) = (++) "\\\"" . showl cs
          showl (c:cs) = showLitChar c . showl cs

instance Bounded Char where
  maxBound = toEnum 1114111
  minBound = toEnum 0

showLitChar :: Char -> ShowS
showLitChar c s = s' ++ s
  where i = fromEnum c
        s' | i == 0x7f = "\\DEL"
           | i < 0x20  = escs i
           | i >= 0x80 = "\\" ++ show i ++ zw
           | otherwise = [c]
        escs j = [ "\\NUL", "\\SOH", "\\STX", "\\ETX", "\\EOT", "\\ENQ", "\\ACK", "\\a"
                 , "\\b", "\\t", "\\n", "\\v", "\\f", "\\r", "\\SO", "\\SI"
                 , "\\DLE", "\\DC1", "\\DC2", "\\DC3", "\\DC4", "\\NAK", "\\SYN", "\\ETB"
                 , "\\CAN", "\\EM", "\\SUB", "\\ESC", "\\FS", "\\GS", "\\RS", "\\US"
                 ] !! j
        zw | null s = ""
           | '0' <= head s && head s <= '9' = ['\\', '&']
           | otherwise = ""

-- Maybe type
data Maybe a = Nothing | Just a deriving (Eq, Show)

maybe              :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  =  n
maybe n f (Just x) =  f x

instance (Ord a) => Ord (Maybe a) where
  Nothing <= _      = True
  Just x  <= Just y = x <= y

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Monad Maybe where
  (Just x) >>= k = k x
  Nothing  >>= k = Nothing
  return         = Just
  fail s         = Nothing

-- Either type
data Either a b = Left a | Right b deriving (Eq, Show)

instance (Ord a, Ord b) => Ord (Either a b) where
  Left  x <= Left y  = x <= y
  Left  _ <= Right _ = True
  Right x <= Right y = x <= y

either               :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x)  =  f x
either f g (Right y) =  g y

-- IO type

instance Functor IO where
  fmap f x = x >>= (return . f)

instance Monad IO where
  return = Prim.retIO
  (>>=)  = Prim.bindIO
  fail s = Prim.failIO s

-- Ordering type

data Ordering = LT | EQ | GT deriving (Eq, Show)
--              deriving (Eq, Ord, Enum, Read, Show, Bounded)

instance Ord Ordering where
  LT <= LT = True
  LT <= EQ = True
  LT <= GT = True
  EQ <= EQ = True
  EQ <= GT = True
  GT <= GT = True
  _  <= _  = False

instance Enum Ordering where
  toEnum x | x == 0 = LT
           | x == 1 = EQ
           | x == 2 = GT
           | otherwise = error "Ordering.toEnum: bad argument"
  fromEnum LT = 0
  fromEnum EQ = 1
  fromEnum GT = 2
  enumFrom o = map toEnum [fromEnum o .. 2]
  enumFromThen o o' = map toEnum [fromEnum o, fromEnum o' .. lastInt]
    where lastInt | o' < o = 0
                  | otherwise = 2

instance Bounded Ordering where
  minBound = LT
  maxBound = GT

instance (Show a) => Show [a] where
  showsPrec p = showList

instance (Show a, Show b) => Show (a, b) where
  show (a, b) = "(" ++ show a ++ "," ++ show b ++ ")"

instance (Show a, Show b, Show c) => Show (a, b, c) where
  show (a, b, c) = "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"

instance (Bounded a, Bounded b) => Bounded (a, b) where
  maxBound = (maxBound, maxBound)
  minBound = (minBound, minBound)

-- Standard numeric types.

instance Eq Int where
  (==) = Prim.intEq

instance Ord Int where
  (<=) = Prim.intLe

instance Num Int where
  (+)  = Prim.intAdd
  (-)  = Prim.intSub
  (*)  = Prim.intMul
  signum = signum''
  fromInteger = Prim.intFromInteger
  abs x | x >= 0    = x
        | otherwise = -x

signum'' :: Int -> Int
signum'' x | x > 0  = 1
           | x == 0 = 0
           | x < 0  = -1

instance Integral Int where
  quotRem = Prim.intQuotRem
  toInteger = Prim.integerFromInt

instance Enum Int where
  succ = (+1)
  pred = (+ (-1))
  toEnum x = x
  fromEnum x = x
  enumFrom n = n : enumFrom (n+1)
  enumFromTo x y | x > y     = []
                 | otherwise = x : enumFromTo (x+1) y
  enumFromThen x y = x : enumFromThen y (2*y-x)
  enumFromThenTo x y z | x == y && z >= x           = x : enumFromThenTo x y z
                       | x == z                     = [x]
                       | compare x y /= compare x z = []
                       | otherwise                  = x : enumFromThenTo y (2*y-x) z


instance Show Int where
  show = Prim.intShow

instance Bounded Int where
  maxBound = Prim.intMaxBound
  minBound = Prim.intMinBound
  
instance Real Int where
  toRational x = toInteger x :% 1

instance Eq Integer where
  (==) = Prim.integerEq

instance Ord Integer where
  (<=) = Prim.integerLe

instance Num Integer where
  (+)  = Prim.integerAdd
  (-)  = Prim.integerSub
  (*)  = Prim.integerMul
  signum = signum'
  fromInteger = id
  abs x | x >= 0    = x
        | otherwise = -x

signum' :: Integer -> Integer
signum' x | x > 0  = 1
          | x == 0 = 0
          | x < 0  = -1

instance Integral Integer where
  quotRem = Prim.integerQuotRem
  toInteger = id

instance Enum Integer where
  succ = (+1)
  pred = (+ (-1))
  toEnum = Prim.integerFromInt
  fromEnum = Prim.intFromInteger
  enumFrom n = n : enumFrom (n+1)
  enumFromTo x y | x > y     = []
                 | otherwise = x : enumFromTo (x+1) y
  enumFromThen x y = x : enumFromThen y (2*y-x)
  enumFromThenTo x y z | x == y && z >= x           = x : enumFromThenTo x y z
                       | x == z                     = [x]
                       | compare x y /= compare x z = []
                       | otherwise                  = x : enumFromThenTo y (2*y-x) z

instance Show Integer where
  show = Prim.integerShow

instance Real Integer where
  toRational x = x :% 1

nonnull     :: (Char -> Bool) -> (String -> [(String, String)])
nonnull p s = [((c:cs), t) | ((c:cs), t) <- [span p s]]

digitToInt :: Char -> Int
digitToInt c
  | isDigit c            = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | otherwise            = error "digitToInt: not a digit"

readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> (String -> [(a, String)])
readInt radix isDig digToInt s =
  [(foldl1 (\n d -> n * radix + d) (map (fromIntegral . digToInt) ds), r)
  | (ds, r) <- nonnull isDig s]

readDec :: (Integral a) => (String -> [(a, String)])
readDec = readInt 10 isDigit digitToInt

readSigned :: (Real a) => (String -> [(a, String)]) -> (String -> [(a, String)])
readSigned readPos = readParen False read'
  where read' r = read'' r ++
                  [(-x, t) | ("-", s) <- lex r,
                             (x, t) <- read'' s]
        read'' r = [(n, s) | (str, s) <- lex r,
                             (n, "") <- readPos str]

instance Read Integer where
  readsPrec p = readSigned readDec

instance Read Int where
  readsPrec p r = [(fromInteger i, t) | (i, t) <- readsPrec p r]

instance Eq Float where
  (==) = Prim.floatEq

instance Ord Float where
  (<=) = Prim.floatLe

instance Num Float where
  (+) = Prim.floatAdd
  (-) = Prim.floatSub
  (*) = Prim.floatMul
  signum = Prim.floatSignum
  fromInteger = Prim.floatFromInteger
  abs x | x >= 0    = x
        | otherwise = -x

instance Fractional Float where
  (/) = Prim.floatDiv
  recip x = (1::Float) / x
  fromRational = Prim.floatFromRational

instance Floating Float where
  pi = 3.1415927
  exp = Prim.floatExp
  log = Prim.floatLog
  (**) = Prim.floatPow
  sin = Prim.floatSin
  cos = Prim.floatCos
  tan = Prim.floatTan
  sinh = Prim.floatSinh
  cosh = Prim.floatCosh
  tanh = Prim.floatTanh
  asin = Prim.floatAsin
  acos = Prim.floatAcos
  atan = Prim.floatAtan
  asinh = Prim.floatAsinh
  acosh = Prim.floatAcosh
  atanh = Prim.floatAtanh

instance Show Float where
  show = Prim.floatShow


properfrac :: (Real a, Fractional a, Integral b) => a -> (b, a)
properfrac x = let (n :% d) = toRational x
                   (q, r) = quotRem n d
                   b = fromInteger q
                   a = fromInteger r / fromInteger d
               in (b, a)

instance RealFrac Float where
  properFraction = properfrac

instance RealFloat Float where
  floatRadix _ = 2
  floatDigits _ = 24
  floatRange _ = (-125,128)
  decodeFloat = Prim.floatDecodeFloat
  encodeFloat m n = fromIntegral m * (2.0 ** fromIntegral n)
  isNaN = Prim.floatIsNaN
  isInfinite = Prim.floatIsInfinite
  isDenormalized = Prim.floatIsDenormalized
  isNegativeZero = Prim.floatIsNegativeZero
  isIEEE _ = True

instance Real Float where
  toRational = Prim.floatToRational

instance Eq Double where
  (==) = Prim.doubleEq

instance Ord Double where
  (<=) = Prim.doubleLe

instance Num Double where
  (+) = Prim.doubleAdd
  (-) = Prim.doubleSub
  (*) = Prim.doubleMul
  signum = Prim.doubleSignum
  fromInteger = Prim.doubleFromInteger
  abs x | x >= 0    = x
        | otherwise = -x

instance Fractional Double where
  (/) = Prim.doubleDiv
  recip x = (1::Double) / x
  fromRational = Prim.doubleFromRational

instance Floating Double where
  pi = 3.141592653589793
  exp = Prim.doubleExp
  log = Prim.doubleLog
  (**) = Prim.doublePow
  sin = Prim.doubleSin
  cos = Prim.doubleCos
  tan = Prim.doubleTan
  sinh = Prim.doubleSinh
  cosh = Prim.doubleCosh
  tanh = Prim.doubleTanh
  asin = Prim.doubleAsin
  acos = Prim.doubleAcos
  atan = Prim.doubleAtan
  asinh = Prim.doubleAsinh
  acosh = Prim.doubleAcosh
  atanh = Prim.doubleAtanh

instance Show Double where
  show = Prim.doubleShow

instance RealFrac Double where
  properFraction = properfrac

instance RealFloat Double where
  floatRadix _ = 2
  floatDigits _ = 53
  floatRange _ = (-1021,1024)
  decodeFloat = Prim.doubleDecodeFloat
  encodeFloat m n = fromIntegral m * (2.0 ** fromIntegral n)
  isNaN = Prim.doubleIsNaN
  isInfinite = Prim.doubleIsInfinite
  isDenormalized = Prim.doubleIsDenormalized
  isNegativeZero = Prim.doubleIsNegativeZero
  isIEEE _ = True


instance Real Double where
  toRational = Prim.doubleToRational

instance Enum Float where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum       = fromInteger . truncate
  enumFrom       = numericEnumFrom
  enumFromThen   = numericEnumFromThen
  enumFromTo     = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

instance Enum Double where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum       = fromInteger . truncate
  enumFrom       = numericEnumFrom
  enumFromThen   = numericEnumFromThen
  enumFromTo     = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

numericEnumFrom         :: (Fractional a) => a -> [a]
numericEnumFromThen     :: (Fractional a) => a -> a -> [a]
numericEnumFromTo       :: (Fractional a, Ord a) => a -> a -> [a]
numericEnumFromThenTo   :: (Fractional a, Ord a) => a -> a -> a -> [a]
numericEnumFrom         =  iterate (+1)
numericEnumFromThen n m =  iterate (+(m-n)) n
numericEnumFromTo n m   =  takeWhile (<= m+1/2) (numericEnumFrom n)
numericEnumFromThenTo n n' m = takeWhile p (numericEnumFromThen n n')
                               where p | n' >= n   = (<= m + (n'-n)/2)
                                       | otherwise = (<= m + (n'-n)/2)

-- Lists

instance Functor [] where
  fmap = map

instance Monad [] where
  m >>= k  = concat (map k m)
  return x = [x]
  fail s   = []

-- Tuples
-- componet projections for pairs:
fst :: (a, b) -> a
fst (x, y) = x

snd :: (a, b) -> b
snd (x, y) = y

-- curry converts an uncurried function to a curried function:
-- uncurry converts a curried function to a function on pairs.
curry       :: ((a, b) -> c) -> a -> b -> c
curry f x y =  f (x, y)

uncurry     :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p =  f (fst p) (snd p)

-- Misc functions

-- until p f yields the result of applying f until p holds.
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x
  | p x       = x
  | otherwise = until p f (f x)

-- asTypeOf is a type-restricted version of const. It is usually used
-- as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same types as the second.
asTypeOf :: a -> a -> a
asTypeOf =  const

-- error stops execution and displays an error message
error :: String -> a
error = Prim.error

-- undefined
undefined :: a
undefined =  error "Prelude.undefined"

-- PreludeList
infixl 9 !!
infixr 5 ++
infix  4 `elem`, `notElem`

-- Map and append
map          :: (a -> b) -> [a] -> [b]
map f []     =  []
map f (x:xs) =  f x : map f xs

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

filter :: (a -> Bool) -> [a] -> [a]
filter p []                 = []
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs 

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

concatMap  :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr ((++) . f) []

-- head and tail extract the first element and remaining elements,
-- respectively, of a list, which must be non-empty.  last and init
-- are dual functions working from the end of a finite list,
-- rather than the beginning.

head           :: [a] -> a
head (x:_)     =  x
head []        =  error "Prelude.head: empty list"

tail             :: [a] -> [a]
tail (_:xs)      =  xs
tail []          =  error "Prelude.tail: empty list"

last        :: [a] -> a
last [x]    =  x
last (_:xs) = last xs
last []     =  error "Prelude.last: empty list"

init        :: [a] -> [a]
init [x]    =  []
init (x:xs) = x : init xs
init []     =  error "Prelude.init: empty list"

null :: [a] -> Bool
null [] = True
null (_:_) = False

-- length returns the length of a finite list as an Int.
length       :: [a] -> Int
length []    =  0
length (_:l) =  1 + length l

-- List index operator, 0-origin
(!!)                :: [a] -> Int -> a
xs     !! n | n < 0 =  error "Prelude.!!: negative index"
[]     !! _         =  error "Prelude.!!: index too large"
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n-1)

-- foldl
foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     =  z
foldl f z (x:xs) =  foldl f (f z x) xs

foldl1          :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) =  foldl f x xs
foldl1 _ []     =  error "Prelude.foldl1: empty list"

scanl        :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs =  q : (case xs of
                       []   -> []
                       x:xs -> scanl f (f q x) xs)

scanl1          :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs) =  scanl f x xs
scanl1 _ []     = []

-- foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
            where go []     = z
                  go (y:ys) = y `k` go ys

foldr1          :: (a -> a -> a) -> [a] -> a
foldr1 f [x]    =  x
foldr1 f (x:xs) =  f x (foldr1 f xs)
foldr1 _ []     =  error "Prelude.foldr1: empty list"

scanr             :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []     =  [q0]
scanr f q0 (x:xs) =  f x q : qs
                     where qs@(q:_) = scanr f q0 xs

scanr1          :: (a -> a -> a) -> [a] -> [a]
scanr1 f []     =  []
scanr1 f [x]    =  [x]
scanr1 f (x:xs) =  f x q : qs
                   where qs@(q:_) = scanr1 f xs

-- iterate
iterate     :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)

-- repeat
repeat   :: a -> [a]
repeat x =  xs where xs = x:xs

-- replicate
replicate     :: Int -> a -> [a]
replicate n x =  take n (repeat x)

-- cycle
cycle    :: [a] -> [a]
cycle [] =  error "Prelude.cycle: empty list"
cycle xs =  xs' where xs' = xs ++ xs'

-- take
take              :: Int -> [a] -> [a]
take n _ | n <= 0 =  []
take n []         =  []
take n (x:xs)     =  x : take (n-1) xs

-- drop
drop               :: Int -> [a] -> [a]
drop n xs | n <= 0 =  xs
drop _ []          = []
drop n (_:xs)      = drop (n-1) xs

-- splitAt
splitAt      :: Int -> [a] -> ([a],[a])
splitAt n xs =  (take n xs, drop n xs)

-- takeWhile, dropWhile, span and break

takeWhile                      :: (a -> Bool) -> [a] -> [a]
takeWhile p []                 =  []
takeWhile p (x:xs) | p x       =  x : takeWhile p xs
                   | otherwise = []

dropWhile           :: (a -> Bool) -> [a] -> [a]
dropWhile p []      =  []
dropWhile p (x:xs') -- todo: xs@(x:xs')
  | p x             =  dropWhile p xs'
  | otherwise       =  x:xs'

span, break :: (a -> Bool) -> [a] -> ([a],[a])
span p [] = ([], [])
span p (x:xs') -- todo: xs@(x:xs')
  | p x = (x:ys, zs)
  | otherwise = ([], x:xs')
  where (ys, zs) = span p xs'

break p = span (not . p)

-- lines, words, unlines and unwords

lines    :: String -> [String]
lines "" =  []
lines s  = let (l, s') = break (== '\n') s
           in l : case s' of
                    []      -> []
                    (_:s'') -> lines s''

-- isSpace = (==' ') -- todo: Char.isSpace

words :: String -> [String]
words s = case dropWhile isSpace s of
            "" -> []
            s' -> w : words s''
              where (w, s'') = break isSpace s'

unlines :: [String] -> String
unlines =  concatMap (++ "\n")

unwords    :: [String] -> String
unwords [] =  ""
unwords ws =  foldr1 (\w s -> w ++ ' ':s) ws

-- reverse
reverse :: [a] -> [a]
reverse =  foldl (flip (:)) []

-- and, or
and, or :: [Bool] -> Bool
and     =  foldr (&&) True
or      =  foldr (||) False

-- any, all
any, all :: (a -> Bool) -> [a] -> Bool
any p    =  or . map p
all p    =  and . map p

-- elem, notElem
elem, notElem :: (Eq a) => a -> [a] -> Bool
elem x        =  any (==x)
notElem x     =  all (/=x)

-- lookup key assocs looks up a ken in an association list.
lookup        :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key [] =  Nothing
lookup key ((x, y) : xys)
  | key == x  =  Just y
  | otherwise =  lookup key xys

-- sum and product
sum, product :: (Num a) => [a] -> a
sum          =  foldl (+) 0
product      =  foldl (*) 1

-- maximum and minimum
maximum, minimum :: (Ord a) => [a] -> a

maximum []       = error "Prelude.maximum: empty list"
maximum xs       = foldl1 max xs

minimum []       = error "Prelude.minimum: empty list"
minimum xs       = foldl1 min xs

-- zip
zip :: [a] -> [b] -> [(a,b)]
zip =  zipWith (,)

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 =  zipWith3 (,,)

zipWith                 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith z (a:as) (b:bs) =  z a b : zipWith z as bs
zipWith _ _ _           = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 z (a:as) (b:bs) (c:cs) = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _                = []

unzip :: [(a, b)] -> ([a], [b])
unzip =  foldr (\(a, b) (as, bs) -> (a:as, b:bs)) ([], []) -- todo: ~

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 =  foldr (\(a, b, c) (as, bs, cs) -> (a:as, b:bs, c:cs)) ([], [], []) -- todo: ~

-- Prelude.Text

type ReadS a = String -> [(a, String)]
type ShowS = String -> String

class Read a where
  readsPrec :: Int -> ReadS a
  readList  :: ReadS [a]
  -- Minimal complete definition: readsSpec
  readList = readParen False (\r -> [pr | ("[", s) <- lex r,
                                          pr       <- readl s])
    where readl  s = [([], t)   | ("]", t) <- lex s] ++
                     [(x:xs, u) | (x, t)   <- reads s,
                                  (xs, u)  <- readl' t]
          readl' s = [([], t)   | ("]", t) <- lex s] ++
                     [(x:xs, v) | (",", t) <- lex s,
                                  (x, u)   <- reads t,
                                  (xs, v)  <- readl' u]

class Show a where
  show      :: a -> String
  showsPrec :: Int -> a -> ShowS
  showList  :: [a] -> ShowS
  -- Minimal complete definition:
  --  show or showsPrec
  showsPrec p x s = show x ++ s
  show x = showsPrec 0 x ""
  showList []     = (++) "[]"
  showList (x:xs) = (:) '[' . shows x . showl xs
    where showl []     = (:) ']'
          showl (x:xs) = (:) ',' . shows x . showl xs

reads :: (Read a) => ReadS a
reads =  readsPrec 0

shows :: (Show a) => a -> ShowS
shows =  showsPrec 0

read   :: (Read a) => String -> a
read s =  case [x | (x, t) <- reads s, ("", "") <- lex t] of
            [x] -> x
            []  -> error "Prelude.read: no parse"
            _   -> error "Prelude.read: ambiguous parse"

showChar :: Char -> ShowS
showChar =  (:)

showString :: String -> ShowS
showString =  (++)

showParen     :: Bool -> ShowS -> ShowS
showParen b p =  if b then showChar '(' . p . showChar ')' else p

readParen     :: Bool -> ReadS a -> ReadS a
readParen b g =  if b then mandatory else optional
  where optional r  = g r ++ mandatory r
        mandatory r = [(x, u) | ("(", s) <- lex r,
                                (x, t)   <- optional s,
                                (")", u) <- lex t]

-- isSpace returns true for any Unicode space character, and the
-- control characters \t, \n, \r, \f, \v
isSpace :: Char -> Bool
isSpace c = let x = fromEnum c
            in (c == ' ') || (x >= 9 && x <= 13)

isDigit, isOctDigit, isHexDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
isOctDigit c = c >= '0' && c <= '7'
isHexDigit c = isDigit c || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'

-- primUnicodeIsUpper
isUpper c = c >= 'A' && c <= 'Z'
isLower c = c >= 'a' && c <= 'z'
isAlpha c = isUpper c || isLower c
isAlphaNum c = isUpper c || isLower c || isDigit c

-- lexLitChar from Haskell 98 language report
lexLitChar :: String -> [(String, String)]
lexLitChar ('\\':s) = map (prefix '\\') (lexEsc s)
  where
    lexEsc (c:s)     | c `elem` "abfnrtv\\\"'" = [([c], s)]
    lexEsc ('^':c:s) | c >= '@' && c <= '_'    = [(['^', c], s)]
    -- Numeric espacpes
    lexEsc ('o':s) = [prefix 'o' (span isOctDigit s)]
    lexEsc ('x':s) = [prefix 'x' (span isHexDigit s)]
    -- lexEsc s@(d:_) | isDigit d = [span isDigit s]
    lexEsc (d:s) | isDigit d = [span isDigit (d:s)]
    -- Very crude approximation to \XYZ.
    lexEsc (c:s) | isUpper c = [span isCharName (c:s)]
    lexEsc _ = []

    isCharName c = isUpper c || isDigit c
    prefix c (t, s) = (c:t, s)

-- lexDigits :: [Char] -> ([Char], [Char])
lexDigits s = [(cs, t) | (cs, t) <- [span isDigit s], cs /= ""]

lex :: ReadS String
lex "" = [("", "")]
lex (c:s) | isSpace c = lex (dropWhile isSpace s)
lex ('\'':s) = [('\'' : ch ++ "'", t) | (ch, '\'': t) <- lexLitChar s, ch /= "'"]
lex ('"':s) = [('"':str, t) | (str, t) <- lexString s]
  where lexString ('"':s) = [("\"", s)]
        lexString s = [(ch++str, u) | (ch, t) <- lexStrItem s, (str, u) <- lexString t]
        lexStrItem ('\\':'&':s) = [("\\&", s)]
        lexStrItem ('\\':c:s) | isSpace c
          = [("\\&", t) | '\\' : t <- [dropWhile isSpace s]]
        lexStrItem s = lexLitChar s
lex (c:s) | isSingle c = [([c],s)]
          | isSym c    = [(c:sym,t) | (sym, t) <- [span isSym s]]
          | isAlpha c  = [(c:nam,t) | (nam, t) <- [span isIdChar s]]
          | isDigit c  = [(c:ds++fe, t) | (ds, s) <- [span isDigit s],
                                          (fe, t) <- lexFracExp s]
          | otherwise  = [] -- bad character
  where isSingle c = c `elem` ",:()[]{}_`"
        isSym c = c `elem` "!@#$%&*+./<=>?\\^|:-~"
        isIdChar c = isAlphaNum c || c `elem` "_'"
        lexFracExp ('.':c:cs) | isDigit c
          = [('.':ds++e, u) | (ds, t) <- lexDigits (c:cs), (e, u) <- lexExp t]
        lexFracExp s = lexExp s
        lexExp (e:s) | e `elem` "eE"
          = [(e:c:ds, u) | (c:t) <- [s], c `elem` "+-", (ds, u) <- lexDigits t] ++
            [(e:ds, t) | (ds, t) <- lexDigits s]
        lexExp s = [("",s)]

----

instance Show () where
  showsPrec p () = showString "()"

instance Read () where
  readsPrec p    = readParen False (\r -> [((), t) | ("(", s) <- lex r
                                                   , (")", t) <- lex s])

instance (Read a) => Read [a] where
  readsPrec p = readList

-- PreludeIO

putChar :: Char -> IO ()
putChar = Prim.putChar

putStr :: String -> IO ()
putStr s = mapM_ putChar s

putStrLn :: String -> IO ()
putStrLn =  Prim.putStrLn

getChar :: IO Char
getChar =  Prim.getChar

getLine :: IO String
getLine =  do c <- getChar
              if c == '\n' then return "" else
                do s <- getLine
                   return (c:s)

print x = putStrLn (show x)

-- Data.Ratio

infixl 7 %

ratPrec = 7 :: Int

reduce _ 0 = error "Ratio.% : zero denominator"
reduce x y = (x `quot` d) :% (y `quot` d)
  where d = gcd x y

(%) :: (Integral a) => a -> a -> Ratio a
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

instance (Integral a) => Real (Ratio a) where
  toRational (x :% y) = toInteger x :% toInteger y

instance (Integral a) => Fractional (Ratio a) where
  (x :% y) / (x' :% y') = (x * y') % (y * x')
  recip (x :% y) = y % x
  fromRational (x :% y) = fromInteger x :% fromInteger y

instance (Integral a) => RealFrac (Ratio a) where
  properFraction (x :% y) = (fromIntegral q, r :% y)
    where (q, r) = quotRem x y
  -- truncate = undefined
  truncate (x :% y) = fromIntegral (x `quot` y)
  round = undefined
  ceiling = undefined
  floor = undefined
  
instance (Integral a) => Enum (Ratio a) where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate
  -- fromEnum (x :% y) = fromIntegral (x `quot` y)
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo
  

