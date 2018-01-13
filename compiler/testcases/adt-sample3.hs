data MyEither a b = MyLeft a | MyRight b

myshow (MyRight x) = "MyRight " ++ show x
myshow (MyLeft x)  = "MyLeft " ++ show x

main = putStrLn $ myshow (MyLeft 10)
