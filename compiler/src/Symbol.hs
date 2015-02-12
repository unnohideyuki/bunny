module Symbol where

type Id = String

type Pos = (Int, Int) -- line number, column number

posLine :: Pos -> Int
posLine (line, _) = line
posCol :: Pos -> Int
posCol (_, col) = col

data Name = Name { name_base :: Id
                 , name_qual :: Id
                 , name_pos  :: Pos
                 }
          deriving Show
