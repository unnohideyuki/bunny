-- ADT: Algebraic Data Type

data Direction = Up | Down
               -- deriving Show

to_s Up   = "Up"
to_s Down = "Down"

main = putStrLn $ to_s Down

