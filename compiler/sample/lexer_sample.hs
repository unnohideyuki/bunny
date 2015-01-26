module Main where
import Lexer

parser :: [Token] -> Alex [Token]
parser ts = do
  t <- alexMonadScan
  case t of
    Eof -> return ts
    _ -> parser (ts ++ [t])

parse :: String -> [Token]
parse s = case runAlex s (parser []) of
  Right xs -> xs
  Left s'  -> error s'

main :: IO ()
main = getContents >>= print . parse
