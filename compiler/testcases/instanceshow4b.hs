data Hoge a b = Hoge a
              | Fuga b

instance (Show a, Show b) => Show (Hoge a b) where
  showsPrec d (Hoge x) = showParen (d > 10) showStr
    where showStr = showString "Hoge " . showsPrec 11 x
  showsPrec d (Fuga y) = showParen (d > 10) showStr
    where showStr = showString "Fuga " . showsPrec 11 y

data X a = X a

instance (Show a) => Show (X a) where
  showsPrec d (X x) = showParen (d > 10) showStr
    where showStr = showString "X " . showsPrec 11 x

x, y :: Hoge Int String
x = Hoge 10
y = Fuga "nine"

main = do
  putStrLn $ show x
  putStrLn $ show y
  putStrLn $ show (X y)
