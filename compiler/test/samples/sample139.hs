data Hoge = Hoge Integer
          | Fuga [Char]
          deriving Show

x = Hoge 10
y = Fuga "nine"

main = do
  putStrLn $ show x
  print y
