data Hoge = Hoge Integer
          | Fuga [Char]

myshow (Hoge n) = "Hoge " ++ show n
myshow (Fuga s) = "Fuga " ++ s

x = Hoge 10
y = Fuga "nine"

main = do
  putStrLn $ myshow x
  putStrLn $ myshow y
