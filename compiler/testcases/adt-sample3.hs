data MyEither a b = MyLeft a | MyRight b

myshow :: MyEithr Int Int -> String
myshow (MyRight x) = "MyRight " ++ show x
myshow (MyLeft x)  = "MyLeft " ++ show x

main = putStrLn $ myshow (MyLeft 10)
