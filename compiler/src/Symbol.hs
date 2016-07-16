module Symbol where

import qualified Data.Map as Map
import Data.Char

type Id = String

type Pos = (Int, Int) -- line number, column number

posLine :: Pos -> Int
posLine (line, _) = line
posCol :: Pos -> Int
posCol (_, col) = col

data Name = Name { orig_name :: Id
                 , qual_name :: Id
                 , name_pos  :: Pos
                 , isConName :: Bool
                 }
          deriving Show

type Table = Map.Map Id

empty :: Table a
empty  = Map.empty

insert         :: Id -> a -> Table a -> Table a
insert s v tab = Map.insert s v tab

tabLookup      :: Id -> Table a -> Maybe a
tabLookup s tab = Map.lookup s tab

fromList :: [(Id, a)] -> Table a
fromList = Map.fromList

escapeId :: Id -> Id
escapeId "" = ""
escapeId (c:cs) = case isAlphaNum c of
  True  -> c : escapeId cs
  False -> "_" ++ (show.ord) c ++ "_" ++ escapeId cs
