module Symbol where

import           Data.Char
import qualified Data.Map.Strict as Map

type Id = String

type Pos = (Int, Int) -- line number, column number

posLine :: Pos -> Int
posLine (line, _) = line
posCol :: Pos -> Int
posCol (_, col) = col

data Name = Name { origName  :: Id
                 , namePos   :: Pos
                 , isConName :: Bool
                 }
          deriving Show

type Table = Map.Map Id

empty :: Table a
empty  = Map.empty

insert :: Id -> a -> Table a -> Table a
insert  = Map.insert

tabLookup :: Id -> Table a -> Maybe a
tabLookup  = Map.lookup

fromList :: [(Id, a)] -> Table a
fromList  = Map.fromList

escapeId :: Id -> Id
escapeId "" = ""
escapeId (c:cs) = if isAlphaNum c
                  then c : escapeId cs
                  else "_" ++ (show.ord) c ++ "_" ++ escapeId cs
