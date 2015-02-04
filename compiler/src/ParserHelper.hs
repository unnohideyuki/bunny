module ParserHelper where

import Lexer
import Absyn

extrPos :: AlexPosn -> Pos
extrPos (AlexPn _ line col) = (line, col)

extrQual :: String -> String -> (String, String)
extrQual qual name =
  case span (/= '.') name of
    (_, "")      -> (qual, name)
    (q, ('.':n)) -> extrQual (qual ++ q ++ ".") n
    (q, n)       -> extrQual (qual ++ q ++ ".") n

mkName :: (String, AlexPosn) -> Name
mkName (s, pos) = Name { name_body = body
                       , name_qual = qual
                       , name_pos  = extrPos pos }
  where
    (qual, body) = extrQual "" s

mkModule n = Module n [] [] []

mkRecField qv _ = RecField qv Exp

mkChar :: (Char, AlexPosn) -> Literal
mkChar (c, pos) = LitChar c $ extrPos pos

mkString :: (String, AlexPosn) -> Literal
mkString (s, pos) = LitString s $ extrPos pos

mkInteger :: (Integer, AlexPosn) -> Literal
mkInteger (i, pos) = LitInteger i $ extrPos pos

mkFloat :: (Float, AlexPosn) -> Literal
mkFloat (d, pos) = LitFloat d $ extrPos pos


