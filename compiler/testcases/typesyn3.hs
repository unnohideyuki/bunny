type T a = a

data F a = F1 (T Int) | F2 Char

instance (Show a) => Show (F a) where
  show (F1 x) = "F1 " ++ show x
  show (F2 y) = "F2 " ++ show y

x :: T Int
x = 10

y :: F (T Int)
y =  F1 10

main = do print y
          print (F2 'a')
