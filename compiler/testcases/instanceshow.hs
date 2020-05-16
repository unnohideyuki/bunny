data Hoge = Hoge Integer
          | Fuga [Char]

instance Show Hoge where
  show (Hoge i) = "Hoge " ++ show i
  show (Fuga s) = "Fuga " ++ show s

x = Hoge 10
y = Fuga "nine"

main = do
  putStrLn $ show x
  putStrLn $ show y
