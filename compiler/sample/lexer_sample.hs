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

pp :: Int -> [Token] -> IO ()
pp _ [] = return ()
pp n (t:ts) = do
  let (s, l) = readTok t
  putStr $ if l > n then "\n" else ""
  putStr s
  putStr " "
  pp l ts
  where
    extrLine (AlexPn _ l _) = l
    readTok tok = case tok of
      TOParen       pos -> ("(", extrLine pos)
      TCParen       pos -> (")", extrLine pos)
      TComma        pos -> (",", extrLine pos)
      TSemi         pos -> (";", extrLine pos)
      TOBrack       pos -> ("[", extrLine pos)
      TCBrack       pos -> ("]", extrLine pos)
      TBackquote    pos -> ("`", extrLine pos)
      TOCurly       pos -> ("{", extrLine pos)
      TCCurly       pos -> ("}", extrLine pos)
      TVOCurly      pos -> ("{", extrLine pos)
      TVCCurly      pos -> ("}", extrLine pos)
      TCase         pos -> ("case", extrLine pos)
      TClass        pos -> ("class", extrLine pos)
      TData         pos -> ("data", extrLine pos)
      TDefault      pos -> ("default", extrLine pos)
      TDeriving     pos -> ("deriving", extrLine pos)
      TDo           pos -> ("do", extrLine pos)
      TElse         pos -> ("else", extrLine pos)
      TForeign      pos -> ("foreign", extrLine pos)
      TIf           pos -> ("if", extrLine pos)
      TImport       pos -> ("import", extrLine pos)
      TIn           pos -> ("in", extrLine pos)
      TInfix        pos -> ("infix", extrLine pos)
      TInfixl       pos -> ("infixl", extrLine pos)
      TInfixr       pos -> ("infixr", extrLine pos)
      TInstance     pos -> ("instance", extrLine pos)
      TLet          pos -> ("let", extrLine pos)
      TModule       pos -> ("module", extrLine pos)
      TNewtype      pos -> ("newtype", extrLine pos)
      TOf           pos -> ("of", extrLine pos)
      TThen         pos -> ("then", extrLine pos)
      TType         pos -> ("type", extrLine pos)
      TWhere        pos -> ("where", extrLine pos)
      TUnderscore   pos -> ("_", extrLine pos)
      TAs           pos -> ("as", extrLine pos)
      THiding       pos -> ("hiding", extrLine pos)
      TQualified    pos -> ("qualified", extrLine pos)
      TSafe         pos -> ("safe", extrLine pos)
      TUnsafe       pos -> ("unsafe", extrLine pos)
      TDotdot       pos -> ("..", extrLine pos)
      TColon        pos -> (":", extrLine pos)
      TDColon       pos -> ("::", extrLine pos)
      TEqual        pos -> ("=", extrLine pos)
      TLam          pos -> ("\\", extrLine pos)
      TVBar         pos -> ("|", extrLine pos)
      TLArrow       pos -> ("<-", extrLine pos)
      TRArrow       pos -> ("->", extrLine pos)
      TAt           pos -> ("@", extrLine pos)
      TTilde        pos -> ("~", extrLine pos)
      TDArrow       pos -> ("=>", extrLine pos)
      TMinus        pos -> ("-", extrLine pos)
      TBang         pos -> ("!", extrLine pos)
      TVarid   (s, pos) -> (s, extrLine pos)
      TConid   (s, pos) -> (s, extrLine pos)
      TVarsym  (s, pos) -> (s, extrLine pos)
      TConsym  (s, pos) -> (s, extrLine pos)
      TQVarid  (s, pos) -> (s, extrLine pos)
      TQConid  (s, pos) -> (s, extrLine pos)
      TQVarsym (s, pos) -> (s, extrLine pos)
      TQConsym (s, pos) -> (s, extrLine pos)
      TInteger (i, pos) -> (show i, extrLine pos)
      TFloat   (x, pos) -> (show x, extrLine pos)
      TString  (s, pos) -> (show s, extrLine pos)
      TChar    (c, pos) -> (show c, extrLine pos)

main :: IO ()
main = getContents >>= pp 1 . parse
