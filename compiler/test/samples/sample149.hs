data Hoge = Hoge Integer
          | Fuga [Char]

x = Hoge 10
y = Fuga "nine"

main = do
  putStrLn $ show x
  putStrLn $ show y

