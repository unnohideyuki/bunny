{
module Lexer where
import Data.Char
}
%wrapper "monadUserState"

-- include for $uniwhite, $unismall, $unilarge, $unisymbol and $unidigit
-- #include "unisets.txt"

-- $whitechar = [$white $uniwhite]
$whitechar = $white

$special = [\( \) \, \; \[ \] \` \{ \}]
$ascsymbol = [\! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~]
--$symbol = [$ascsymbol $unisymbol] # [$special _ \" \']
$symbol = $ascsymbol # [$special _ \" \']

-- $small = [a-z _ $unismall]
-- $large = [A-Z $unilarge]
-- $digit = [0-9 $unidigit]
$small = [a-z _]
$large = [A-Z]
$digit = [0-9]
$octit = [0-7]
$hexit = [0-9a-fA-F]

$graphic = [$small $large $symbol $digit $special \" \' :]

@varid = ($small [$small $large $digit \']*)
@conid = ($large [$small $large $digit \']*)
@varsym = ($symbol [$symbol :]*)
@consym = (: [$symbol :]*)
@qual = (@conid \.)+

@integer = ($digit+ | 0o $octit+ | 0O $octit+ | 0x $hexit+ | 0X $hexit+)
@exponent = ([eE] [\+ \-]? $digit+)
@float = ($digit+ "." $digit+ @exponent? | $digit+ @exponent?)

$charesc = [a b f n r t v \\ \" \']
$cntrl = [A-Z \@ \[ \\ \] \^ _]
@ascii = ("^" $cntrl 
        | "NUL" | "SOH" | "STX" | "ETX" | "EOT" | "ENQ" | "ACK" | "BEL" 
        | "BS"  | "HT"  | "LF"  | "VT"  | "FF"  | "CR"  | "SO"  | "SI" 
        | "DLE" | "DC1" | "DC2" | "DC3" | "DC4" | "NAK" | "SYN" | "ETB" 
        | "CAN" | "EM"  | "SUB" | "ESC" | "FS"  | "GS"  | "RS"  | "US" 
        | "SP"  | "DEL" )
@escape = \\ ($charesc | @ascii | $digit+ | "o" $octit+ | "x" $hexit+)

@rsvdid = ( "case" | "class"  | "data"    | "default" | "deriving"
          | "do"   | "else"   | "foreign" | "if"      | "import"
          | "in"   | "infix"  | "infixl"  | "infixr"  | "instance"
          | "let"  | "module" | "newtype" | "of"      | "then"
          | "type" | "where"  | "_"       )

@rsvdop = ( ".." | ":" | "::" | "=" | "\\" | "|" | "<-" | "->"
          | "@" | "~" | "=>" )

haskell :-

-- white spaces
<0> $whitechar                  { act_white_space }

-- comments
<0,comment> "{-"                { act_open_comment }
<comment> . | \n                { act_nested_comment }

<0> "--"\-* [^$symbol :] .*     { skip }
<0> "--"\-*  / {eoleof}         { skip }

-- string
<0> \"                          { act_open_string }
<string> \"                     { act_close_string }
<string> $graphic # [\" \\]     { act_string }
<string> " "                    { act_string }
<string> "\\&"                  { skip }
<string> @escape                { act_esc_string }
<string> \\ $whitechar+ \\      { skip }
<string> [^\']                  { act_string }

-- char
<0> \' $graphic # [\' \\] \'    { act_char }
<0> \' " " \'                   { act_char }
<0> \' @escape \'               { act_esc_char }
<0> \' [^\'] \'                 { act_char }

-- special symbols
<0> "("                         { act_token1 TOParen }
<0> ")"                         { act_token1 TCParen }
<0> ","                         { act_token1 TComma }
<0> ";"                         { act_token1 TSemi }
<0> "["                         { act_token1 TOBrack }
<0> "]"                         { act_token1 TCBrack }
<0> "`"                         { act_token1 TBackquote }
<0> "{"                         { act_ocurly }
<0> "}"                         { act_ccurly }

-- reserved id 
<0> "case"                      { act_token1 TCase }
<0> "class"                     { act_token1 TClass }
<0> "data"                      { act_token1 TData }
<0> "default"                   { act_token1 TDefault }
<0> "deriving"                  { act_token1 TDeriving }
<0> "else"                      { act_token1 TElse }
<0> "foreign"                   { act_token1 TForeign }
<0> "if"                        { act_token1 TIf }
<0> "import"                    { act_token1 TImport }
<0> "in"                        { act_token1 TIn }
<0> "infix"                     { act_token1 TInfix }
<0> "infixl"                    { act_token1 TInfixl }
<0> "infixr"                    { act_token1 TInfixr }
<0> "instance"                  { act_token1 TInstance }
<0> "module"                    { act_token1 TModule }
<0> "newtype"                   { act_token1 TNewtype }
<0> "then"                      { act_token1 TThen }
<0> "type"                      { act_token1 TType }
<0> "_"                         { act_token1 TUnderscore }

<0> "do"                        { act_layout_keyword TDo }
<0> "let"                       { act_layout_keyword TLet }
<0> "of"                        { act_layout_keyword TOf }
<0> "where"                     { act_layout_keyword TWhere }

-- named keywords (not reserved)
<0> "as"                        { act_token1 TAs }
<0> "hiding"                    { act_token1 THiding }
<0> "qualified"                 { act_token1 TQualified }
<0> "safe"                      { act_token1 TSafe }
<0> "unsafe"                    { act_token1 TUnsafe }

-- reserved op
<0> ".."                        { act_token1 TDotdot }
<0> ":"                         { act_token1 TColon }
<0> "::"                        { act_token1 TDColon }
<0> "="                         { act_token1 TEqual }
<0> \\                          { act_token1 TLam }
<0> "|"                         { act_token1 TVBar }
<0> "<-"                        { act_token1 TLArrow }
<0> "->"                        { act_token1 TRArrow }
<0> "@"                         { act_token1 TAt }
<0> "~"                         { act_token1 TTilde }
<0> "=>"                        { act_token1 TDArrow }

-- named symbols (not reserved)
<0> "-"                         { act_token1 TMinus }
<0> "!"                         { act_token1 TBang }

<0> @varid                      { act_token2 TVarid }
<0> @conid                      { act_token2 TConid }
<0> @varsym                     { act_token2 TVarsym }
<0> @consym                     { act_token2 TConsym }

<0> @qual @rsvdid               { \_ _ -> alexError "syntax error" }
<0> @qual @rsvdop               { \_ _ -> alexError "syntax error" }
<0> @qual @varid                { act_token2 TQVarid }
<0> @qual @conid                { act_token2 TQConid }
<0> @qual @varsym               { act_token2 TQVarsym }
<0> @qual @consym               { act_token2 TQConsym }

<0> @integer                    { act_integer }
<0> @float                      { act_float }

{
data Token = 
           -- special symbols
             TOParen AlexPosn
           | TCParen AlexPosn
           | TComma AlexPosn
           | TSemi AlexPosn
           | TOBrack AlexPosn
           | TCBrack AlexPosn
           | TBackquote AlexPosn
           | TOCurly AlexPosn 
           | TCCurly AlexPosn
           | TVOCurly AlexPosn
           | TVCCurly AlexPosn
           -- reservedids
           | TCase AlexPosn
           | TClass AlexPosn
           | TData AlexPosn
           | TDefault AlexPosn
           | TDeriving AlexPosn
           | TDo AlexPosn
           | TElse AlexPosn
           | TForeign AlexPosn
           | TIf AlexPosn
           | TImport AlexPosn
           | TIn AlexPosn
           | TInfix AlexPosn
           | TInfixl AlexPosn
           | TInfixr AlexPosn
           | TInstance AlexPosn
           | TLet AlexPosn
           | TModule AlexPosn
           | TNewtype AlexPosn
           | TOf AlexPosn           
           | TThen AlexPosn
           | TType AlexPosn
           | TWhere AlexPosn
           | TUnderscore AlexPosn
           -- named keywords (not reserved)
           | TAs AlexPosn
           | THiding AlexPosn
           | TQualified AlexPosn
           | TSafe AlexPosn
           | TUnsafe AlexPosn
           -- reserved ops
           | TDotdot AlexPosn
           | TColon AlexPosn
           | TDColon AlexPosn
           | TEqual AlexPosn
           | TLam AlexPosn
           | TVBar AlexPosn
           | TLArrow AlexPosn
           | TRArrow AlexPosn
           | TAt AlexPosn
           | TTilde AlexPosn
           | TDArrow AlexPosn
           -- named symbols (not reserved)
           | TMinus AlexPosn
           | TBang AlexPosn
           -- varid, conid, varsym, consym etc.
           | TVarid (String, AlexPosn)
           | TConid (String, AlexPosn)
           | TVarsym (String, AlexPosn)
           | TConsym (String, AlexPosn)
           | TQVarid (String, AlexPosn)
           | TQConid (String, AlexPosn)
           | TQVarsym (String, AlexPosn)
           | TQConsym (String, AlexPosn)
           -- Literals
           | TInteger (Integer, AlexPosn)
           | TFloat (Double, AlexPosn)
           | TString (String, AlexPosn)
           | TChar (Char, AlexPosn)
           | Eof
             deriving (Show)

data AlexUserState = AlexUserState { bof :: Bool
                                   , morrow :: Bool
                                   , layout_ctx :: [Int]
                                   , pending_toks :: [Token]
                                   , str :: String
                                   , str_pos :: AlexPosn
                                   , comment_depth :: Int
                                   , last_char :: Char
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { bof = True
                                  , morrow = False
                                  , layout_ctx = []
                                  , pending_toks = []
                                  , str = ""
                                  , str_pos = (AlexPn 0 0 0)
                                  , comment_depth = 0
                                  , last_char = '\0'
                                  }

eoleof :: user -> AlexInput -> Int -> AlexInput -> Bool
eoleof _ _ _ (_, _, _, s) =
  case s of
    []       -> True -- end of file
    ('\n':_) -> True -- end of line
    _ -> False

-- State Utilities

getBof :: Alex Bool
getBof = Alex $ \st@AlexState{alex_ust=ust} -> Right (st, bof ust)

setBof :: Bool -> Alex ()
setBof b = Alex $ \st@AlexState{alex_ust=ust} ->
  Right (st{alex_ust=ust{bof=b}}, ())

getMorrow :: Alex Bool
getMorrow = Alex $ \st@AlexState{alex_ust=ust} -> 
  Right (st, morrow ust)

setMorrow :: Bool -> Alex ()
setMorrow b = Alex $ \st@AlexState{alex_ust=ust} -> 
  Right (st{alex_ust=ust{morrow=b}}, ())

unFlags :: Alex ()
unFlags = do setBof False; setMorrow False

getLayoutCtx :: Alex [Int]
getLayoutCtx = Alex $ \st@AlexState{alex_ust=ust} -> 
  Right (st, layout_ctx ust)

setLayoutCtx :: [Int] -> Alex ()
setLayoutCtx cs = Alex $ \st@AlexState{alex_ust=ust} ->
  Right (st{alex_ust=ust{layout_ctx=cs}}, ())

getPendingToks :: Alex [Token]
getPendingToks = Alex $ \st@AlexState{alex_ust=ust} -> 
  Right (st, pending_toks ust)

setPendingToks :: [Token] -> Alex ()
setPendingToks ts = Alex $ \st@AlexState{alex_ust=ust} ->
  Right (st{alex_ust=ust{pending_toks=ts}}, ())

getStr :: Alex String
getStr = Alex $ \st@AlexState{alex_ust=ust} -> 
  Right (st, str ust)

setStr :: String -> Alex ()
setStr s = Alex $ \st@AlexState{alex_ust=ust} -> 
  Right (st{alex_ust=ust{str=s}}, ())

getStrPos :: Alex AlexPosn
getStrPos = Alex $ \st@AlexState{alex_ust=ust} -> 
  Right (st, str_pos ust)

setStrPos :: AlexPosn -> Alex ()
setStrPos pos = Alex $ \st@AlexState{alex_ust=ust} -> 
  Right (st{alex_ust=ust{str_pos=pos}}, ())

appendChar :: Char -> Alex ()
appendChar c = do s <- getStr; setStr $ s ++ [c]

getCommentDepth :: Alex Int
getCommentDepth = Alex $ \st@AlexState{alex_ust=ust} -> 
  Right (st, comment_depth ust)

setCommentDepth :: Int -> Alex ()
setCommentDepth d = Alex $ \st@AlexState{alex_ust=ust} ->
   Right (st{alex_ust=ust{comment_depth=d}}, ())

getLastChar :: Alex Char
getLastChar = Alex $ \st@AlexState{alex_ust=ust} -> 
  Right (st, last_char ust)

setLastChar :: Char -> Alex ()
setLastChar c = Alex $ \st@AlexState{alex_ust=ust} ->
  Right (st{alex_ust=ust{last_char=c}}, ())

nop :: Alex ()
nop = return ()

alexEOF :: Alex Token
alexEOF = do
  depth <- getCommentDepth
  toks <- getPendingToks
  if depth == 0 then
    if length toks == 0 then
      return Eof
    else
      alexError $ "Pending tokens: " ++ show toks
  else
    alexError $ "Unterminated comment: " ++ show depth

appendToken :: Token -> Alex ()
appendToken t = Alex $ \st ->
  let
    ust = alex_ust st
    toks = pending_toks ust
    ust' = ust{pending_toks=toks ++ [t]}
    inp = alex_inp st
    inp' = (' ':inp)
    st' = st{alex_inp=inp', alex_ust=ust'}
  in
   Right (st', ())

moveColumn :: Int -> Alex ()
moveColumn x = Alex $ \st ->
  let
    (AlexPn offset line col) = alex_pos st
  in
   Right (st{alex_pos=AlexPn offset line (col + x)}, ())

pushCtx :: Int -> Alex ()
pushCtx m = do
  ms <- getLayoutCtx
  setLayoutCtx (m:ms)

popCtx :: Alex ()
popCtx = do
  ms <- getLayoutCtx
  case ms of
    [] -> alexError "Cannot pop from empty layout context."
    (_:ms') -> setLayoutCtx ms'

peepCtx :: Alex Int
peepCtx = do
  ms <- getLayoutCtx
  case ms of
    [] -> return (-1)
    (m:_) -> return m

-- Token Actions

act_white_space :: AlexAction Token
act_white_space _ _ = do
  toks <- getPendingToks
  case toks of
    (t:ts) -> do setPendingToks ts
                 moveColumn (-1)
                 return t
    [] -> alexMonadScan

act_open_comment :: AlexAction Token
act_open_comment _ _ = do
  depth <- getCommentDepth
  setCommentDepth $ depth + 1
  alexSetStartCode comment
  alexMonadScan

act_nested_comment :: AlexAction Token
act_nested_comment (_, _, _, (c:_)) _ = do
  c' <- getLastChar
  setLastChar c
  case (c':c:[]) of
    "{-" -> incDepth
    "-}" -> decDepth
    _ -> nop
  alexMonadScan
  where
    incDepth :: Alex ()
    incDepth = do d <- getCommentDepth; setCommentDepth (d + 1)
    decDepth :: Alex ()
    decDepth = do d <- getCommentDepth
                  setCommentDepth (d - 1)
                  alexSetStartCode $ if d > 1 then comment else 0

act_open_string :: AlexAction Token
act_open_string (pos, _, _, _)  _ =
  do setStr ""
     setStrPos pos
     alexSetStartCode string
     alexMonadScan

act_close_string :: AlexAction Token
act_close_string _ _ =
  do s <- getStr
     pos <- getStrPos
     alexSetStartCode 0
     act_token (TString (s, pos)) pos

act_string :: AlexAction Token
act_string (_, _, _, (c:_)) _ = do appendChar c; alexMonadScan

esc2Char :: String -> Maybe Char
esc2Char ('^':c:[]) = if c >= '@' && c <= '_'
                      then Just (chr (ord c - ord '@'))
                      else Nothing
esc2Char s = lookup s xs where
  xs =[("a",   '\a'),   ("b",   '\b'),   ("f",   '\f'),   ("n",   '\n'), 
       ("r",   '\r'),   ("t",   '\t'),   ("v",   '\v'),   ("\\",  '\\'), 
       ("\"",  '\"'),   ("\'",  '\''),
       ("NUL", '\NUL'), ("SOH", '\SOH'), ("STX", '\STX'), ("ETX", '\ETX'),
       ("EOT", '\EOT'), ("ENQ", '\ENQ'), ("ACK", '\ACK'), ("BEL", '\BEL'),
       ("BS",  '\BS'),  ("HT",  '\HT'),  ("LF",  '\LF'),  ("VT",  '\VT'),
       ("FF",  '\FF'),  ("CR",  '\CR'),  ("SO",  '\SO'),  ("SI",  '\SI'),
       ("DLE", '\DLE'), ("DC1", '\DC1'), ("DC2", '\DC2'), ("DC3", '\DC3'),
       ("DC4", '\DC4'), ("NAK", '\NAK'), ("SYN", '\SYN'), ("ETB", '\ETB'),
       ("CAN", '\CAN'), ("EM",  '\EM'),  ("SUB", '\SUB'), ("ESC", '\ESC'),
       ("FS",  '\FS'),  ("GS",  '\GS'),  ("RS",  '\RS'),  ("US",  '\US'),
       ("SP",  '\SP'),  ("DEL", '\DEL')]

act_esc_string :: AlexAction Token
act_esc_string (_, _, _, s) len = do 
  case esc2Char esc of
    Just c -> appendChar c
    Nothing -> alexError $ "lexical error in string literal, " ++ esc
  alexMonadScan
  where
    (_:esc) = take len s

act_char :: AlexAction Token
act_char (pos, _, _, (_:(c:_))) _ = act_token (TChar (c, pos)) pos

act_esc_char :: AlexAction Token
act_esc_char (pos, _, _, s) len = do
  case esc2Char esc of
    Just c -> return $ TChar (c, pos)
    Nothing -> alexError $ "lexical error in char literal, " ++ show esc
  where
    -- "'\\xx'" to "xx"
    esc = drop 2 $ take (len - 1) s

act_ocurly :: AlexAction Token
act_ocurly (pos, _, _, _) _ = 
  do unFlags; pushCtx 0; return $ TOCurly pos

act_ccurly :: AlexAction Token
act_ccurly (pos, _, _, _) _ = 
  do unFlags; popCtx; return $ TCCurly pos

act_layout_keyword :: (AlexPosn -> Token) -> AlexAction Token
act_layout_keyword maker (pos, _, _, _) _ =
  do tok <- act_token (maker pos) pos
     setMorrow True
     return tok

act_token :: Token -> AlexPosn -> Alex Token
act_token tok pos@(AlexPn _ _ n) = do
  morr <- getMorrow
  beg <- getBof
  unFlags
  m <- peepCtx
  if (beg && not moduleTok) || morr then
    if n > m then
      do pushCtx n; appendToken tok; return $ TVOCurly pos
    else
      do appendToken $ TVOCurly pos; appendToken $ TVCCurly pos; layout
  else
    layout
  where
    moduleTok = case tok of {TModule _ -> True; _ -> False}
    layout :: Alex Token
    layout = do
      m' <- peepCtx
      if n < m' then
        do popCtx; appendToken $ TVCCurly pos; layout
      else
        do if n == m' then
             do appendToken $ TSemi pos; appendToken tok
           else
             appendToken tok
           alexMonadScan

act_token1 :: (AlexPosn -> Token) -> AlexAction Token
act_token1 maker (pos@(AlexPn _ _ n), _, _, _) _ =
  act_token (maker pos) pos

act_token2 :: ((String, AlexPosn) -> Token) -> AlexAction Token
act_token2 maker inp@(_, _, _, s) len = 
  act_token1 (\pos -> maker $ (take len s, pos)) inp len

act_integer :: AlexAction Token
act_integer (pos, _, _, s) len = act_token (TInteger (i, pos)) pos
  where
    i = read $ take len s  

act_float :: AlexAction Token
act_float (pos, _, _, s) len = do act_token (TFloat (x, pos)) pos
  where
    x = read $ take len s  
}
