module ParserHelper where

import Lexer
import Absyn

extrPos :: AlexPosn -> Pos
extrPos (AlexPn _ line col) = (line, col)

extrQual qual name =
  case span (/= '.') name of
    (_, "")      -> (qual, name)
    (q, ('.':n)) -> extrQual (qual ++ q ++ ".") n
    (q, n)       -> extrQual (qual ++ q ++ ".") n

mkName (s, pos) = Name { name_body = body
                       , name_qual = qual
                       , name_pos  = extrPos pos }
  where
    (qual, body) = extrQual "" s

mkModule modid = Module modid []
