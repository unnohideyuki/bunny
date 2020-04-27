module Prelude where

infixr 5 :

class Show a where
  show :: a -> [Char]

data Bool = False | True

instance Show Bool where
  show = Prim.showConName

instance (Show a) => Show [a] where
  show []     = "[]"
  show [x]    = "[" ++ show x ++ "]"
  show (x:xs) = "[" ++ foldl (\s t -> s ++ "," ++ show t) (show x) xs ++ "]"

-- instance (Show a, Show b) => Show (a, b) where
--   show (a, b) = "(" ++ show a ++ "," ++ show b ++ ")"

infixl 7 *, /, `quot`, `rem`, `div`, `mod`
infixl 6 +, -

infixl 1 >>, >>=

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  fail   :: String -> m a

  -- Minimal complete definition: (>>=), return
  p >> q = p >>= \_ -> q
  fail s = error s

instance Monad IO where
  return = Prim.retIO
  (>>=)  = Prim.bindIO
  fail s = Prim.failIO s

infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f (g x)

infixr 5 ++
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

foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     =  z
foldl f z (x:xs) =  foldl f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
            where go []     = z
                  go (y:ys) = y `k` go ys

infix 4 ==, /=, <, <=, >=, >
infixr 3 &&

not :: Bool -> Bool
not True  = False
not False = True

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

-- Equality and Ordered classes

class Eq a where
  (==),(/=) :: a -> a -> Bool
  -- Minimal Complete Definition:
  -- (==) or (/=)
  x /= y = not (x == y)
  x == y = not (x /= y)

class (Eq a) => Ord a where
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  compare :: a -> a -> Ordering
  max :: a -> a -> a
  min :: a -> a -> a
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

instance Ord Char where
  (<=) = Prim.charLe

instance Eq Char where
  (==) = Prim.charEq

instance Show Char where
  -- todo: escape
  show c = ['\'', c, '\'']

class (Eq a) => Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  negate x = 0 - x

instance Num Integer where
  (+)  = Prim.integerAdd
  (-)  = Prim.integerSub
  (*)  = Prim.integerMul

instance Ord Integer where
  (<=) = Prim.integerLe

instance Eq Integer where
  (==) = Prim.integerEq

instance Show Integer where
  show = Prim.integerShow

instance Num Int where
  (+)  = Prim.intAdd
  (-)  = Prim.intSub
  (*)  = Prim.intMul

instance Ord Int where
  (<=) = Prim.intLe

instance Eq Int where
  (==) = Prim.intEq

instance Show Int where
  show = Prim.intShow

print x = putStrLn (show x)

otherwise = True
