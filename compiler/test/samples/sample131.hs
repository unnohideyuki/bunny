-- from Programming Haskell section 10.3

data Nat = Zero | Succ Nat

myShow Zero = "Zero"
myShow (Succ n) = "Succ " ++ myShow n

main = putStrLn $ myShow (Succ Zero)
