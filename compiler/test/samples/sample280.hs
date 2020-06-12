data Hoge a b = Hoge a
              | Fuga b
              deriving Show

x, y :: Hoge Int String
x = Hoge 10
y = Fuga "nine"

main = do
  putStrLn $ show x
  putStrLn $ show y
