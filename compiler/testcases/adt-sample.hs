-- ADT: Algbraic Data Type

data Direction = Right | Left | Up | Down
               -- deriving Show

to_s Up    = "Up"
to_s Left  = "Left"
to_s Right = "Right"
to_s Down  = "Down"

main = putStrLn $ to_s Right

