data Hoge = Hoge Int
          | Fuga [Char]

myshow (Hoge n) = "Hoge " ++ show n
myshow (Fuga s) = "Fuga " ++ s

t :: Num a => a
t = 10

x = Hoge t
y = Fuga "nine"

main = do
  putStrLn $ myshow x
  putStrLn $ myshow y
