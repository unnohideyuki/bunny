module Symbol where

import qualified Data.Map as Map

type Id = String

type Pos = (Int, Int) -- line number, column number

posLine :: Pos -> Int
posLine (line, _) = line
posCol :: Pos -> Int
posCol (_, col) = col

data Name = Name { orig_name :: Id
                 , qual_name :: Id
                 , name_pos  :: Pos
                 }
          deriving Show

type Table = Map.Map Id

empty :: Table a
empty  = Map.empty

insert         :: Id -> a -> Table a -> Table a
insert s v tab = Map.insert s v tab

lookup       :: Id -> Table a -> Maybe a
lookup s tab = Map.lookup s tab

fromList :: [(Id, a)] -> Table a
fromList = Map.fromList
