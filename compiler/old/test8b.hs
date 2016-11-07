f x y = case x > y of
  True -> "True"
  False -> "False"

-- main = putStrLn (f 100 10)
main = putStrLn (f 'b' 'x')

not :: Bool -> Bool
not True = False
not False = True

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

instance Ord Char where
  (<)  = Prim.charLt
  (<=) = Prim.charLe
  (>=) = Prim.charGe
  (>)  = Prim.charGt

instance Eq Char where
  (==) = Prim.charEq

class (Eq a) => Num a where
  (+) :: a -> a -> a

instance Ord Integer where
  (<)  = Prim.integerLt
  (<=) = Prim.integerLe
  (>=) = Prim.integerGe
  (>)  = Prim.integerGt

instance Num Integer where
  (+)  = Prim.integerEq
