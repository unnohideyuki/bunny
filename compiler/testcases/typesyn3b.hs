type T a = a

data F a = F1 (T Int) | F2 Char deriving Show

x :: T Int
x = 10

y :: F (T Int)
y =  F1 10

main = do print y
          print (F2 'a')
