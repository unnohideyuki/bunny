data Hoge = Hoge Integer
          | Fuga [Char]

myshow (Hoge n) = "Hoge " ++ show n
myshow (Fuga s) = "Fuga " ++ s

t = 10

x = Hoge t
y = Fuga "nine"

main = do
  putStrLn $ myshow x
  putStrLn $ myshow y

