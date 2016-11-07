f x y = x > y

g True = "True"
g False = "False"

main = putStrLn (g (f 'b' 'x'))

not :: Bool -> Bool
not True = False
not False = True

class Eq a where
  (==),(/=) :: a -> a -> Bool
  -- Minimal Complete Definition:
  -- (==) or (/=)
  x /= y = not (x == y)
  x == y = not (x /= y)

-- class (Eq a) => Ord a where
class Ord a where
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  (>) :: a -> a -> Bool

instance Ord Char where
  (<)  = Prim.charLt
  (<=) = Prim.charLe
  (>=) = Prim.charGe
  (>)  = Prim.charGt

class (Eq a) => Num a where
  (+) :: a -> a -> a

instance Ord Integer where
  (<)  = Prim.integerLt
  (<=) = Prim.integerLe
  (>=) = Prim.integerGe
  (>)  = Prim.integerGt

instance Num Integer where
  (+)  = Prim.integerEq
