module Prelude where

infixr 9 .
infixl 7 *, /, `quot`, `rem`, `div`, `mod`
infixl 6 +, -

infixr 5 :
infix  4 ==, /=, <, <=, >=, >
infixr 3 &&
infixl 1 >>, >>=
infixr 0 $

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

-- todo: Show cannot be declare after Num
class Show a where
  show      :: a -> [Char]
  showsPrec :: Int -> a -> ([Char] -> [Char])
  showList  :: [a] -> ([Char] -> [Char])
  -- Minimal complete definition:
  --  show or showsPrec
  showsPrec p x s = show x ++ s
  show x = showsPrec 0 x ""
  showList []     = (++) "[]"
  showList (x:xs) = (:) '[' . showsPrec 0 x . showl xs
    where showl []     = (:) ']'
          showl (x:xs) = (:) ',' . showsPrec 0 x . showl xs

-- Numeric classes

class (Eq a, Show a) => Num a where
  (+), (-), (*) :: a -> a -> a
  negate        :: a -> a
  abs, signum   :: a -> a
  fromInteger   :: Integer -> a
  -- Minimal complete definition:
  --  All, except negate or (-)
  x - y    = x + nagate y
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

-- Monadic classes

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  return :: a -> m a
  fail   :: [Char] -> m a
  -- Minimal complete definition: (>>=), return
  m >> k = m >>= \_ -> k
  fail s = error s

sequence :: Monad m => [m a] -> m [a]
sequence =  foldr mcons (return [])
  where mcons p q = p >>= \x -> q >>= \y -> return (x:y)

sequence_ :: Monad m => [m a] -> m ()
sequence_ =  foldr (>>) (return ())

-- Function type

-- identity function
id   :: a -> a
id x = x

-- constant function

-- function composition
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

-- flip f takes its (first) two arguments in the reverse order of f.
flip       :: (a -> b -> c) -> b -> a -> c
flip f x y =  f y x

-- right-associating infix application operators
-- (useful in continuation-passing style)
($) :: (a -> b) -> a -> b
f $ x = f x

-- Boolean type

data Bool = False | True

-- Boolean functions

(&&) :: Bool -> Bool -> Bool
True  && x = x
False && x = False

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

instance Show Char where
  -- todo: escape
  show c = ['\'', c, '\'']
  showList cs = (:) '"' . showl cs
    where showl "" = (:) '"'
          -- showl ('"':cs) = (++) "\\\"" . showl cs
          showl (c:cs) = showLitChar c . showl cs

-- todo: should convert to printable characters
showLitChar c = (++) [c]

-- Maybe type
data Maybe a = Nothing | Just a

instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x  == Just y  = x == y
  _       == _       = False

instance (Ord a) => Ord (Maybe a) where
  Nothing <= _      = True
  Just x  <= Just y = x <= y

instance (Show a) => Show (Maybe a) where
  show Nothing  = "Nothing"
  show (Just x) = "Just " ++ show x

instance Monad Maybe where
  (Just x) >>= k = k x
  Nothing  >>= k = Nothing
  return         = Just
  fail s         = Nothing

{-
instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = f x

-}
-- Either type

-- IO type

instance Monad IO where
  return = Prim.retIO
  (>>=)  = Prim.bindIO
  fail s = Prim.failIO s

-- Ordering type

data Ordering = LT | EQ | GT
--              deriving (Eq, Ord, Enum, Read, Show, Bounded)

instance Show Ordering where
  show = Prim.showConName

instance Eq Ordering where
  a == b = case (a, b) of
    (LT, LT) -> True
    (LT, EQ) -> False
    (LT, GT) -> False
    (EQ, LT) -> False
    (EQ, EQ) -> True
    (EQ, GT) -> False
    (GT, LT) -> False
    (GT, EQ) -> False
    (GT, GT) -> True

instance Ord Ordering where
  a <= b = case (a, b) of
    (LT, LT) -> True
    (LT, EQ) -> True
    (LT, GT) -> True
    (EQ, LT) -> False
    (EQ, EQ) -> True
    (EQ, GT) -> True
    (GT, LT) -> False
    (GT, EQ) -> False
    (GT, GT) -> True

instance Show Bool where
  show = Prim.showConName

instance (Show a) => Show [a] where
  showsPrec p = showList

instance (Show a, Show b) => Show (a, b) where
  show (a, b) = "(" ++ show a ++ "," ++ show b ++ ")"

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

instance Show Float where
  show = Prim.floatShow

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

instance Show Double where
  show = Prim.doubleShow

-- Lists
-- Functor, Monad

-- Tuples
-- componet projections for pairs:
fst :: (a, b) -> a
fst (x, y) = x

snd :: (a, b) -> b
snd (x, y) = y

-- PreludeList
infixr 5 ++

-- Map and append
(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

concatMap  :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr ((++) . f) []

head             :: [a] -> a
head (x:xs)      =  x -- todo: wild card
head []          =  error "Prelude.head: empty list"

tail             :: [a] -> [a]
tail (x:xs)      =  xs -- todo wild card
tail []          =  error "Prelude.tail: empty list"

-- foldl
foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     =  z
foldl f z (x:xs) =  foldl f (f z x) xs

-- foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
            where go []     = z
                  go (y:ys) = y `k` go ys

-- take
take              :: Int -> [a] -> [a]
take n _ | n <= 0 =  []
take n []         =  []
take n (x:xs)     =  x : take (n-1) xs

-- takeWhile ...

-- PreludeIO

print x = putStrLn (show x)

