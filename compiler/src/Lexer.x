{
module Lexer where
}
%wrapper "monadUserState"

$ascSymbol = [\! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~]
$symbol = $ascSymbol

$small = [a-z]
$large = [A-Z]
$digit = [0-9]
$octit = [0-7]
$hexit = [0-9a-fA-F]

@varid = ($small [$small $large $digit \']*)
@conid = ($large [$small $large $digit \']*)
@varsym = ($symbol [$symbol :]*)
@consym = (: [$symbol :]*)
@qual = (@conid \.)+

@integer = ($digit+ | 0o $octit+ | 0O $octit+ | 0x $hexit+ | 0X $hexit+)
@exponent = ([eE] [\+ \-]? $digit+)
@float = ($digit+ "." $digit+ @exponent? | $digit+ @exponent?)

haskell :-

<0> $white                      { act_white_space }

<0,comment> "{-"                { act_open_comment }
<comment> . | \n                { act_nested_comment }

<0> "--"\-* [^$symbol :] .*     { skip }
<0> "--"\-* [\ \t]* $           { skip }

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
<0> "as"                        { act_token1 TAs }
<0> "case"                      { act_token1 TCase }
<0> "class"                     { act_token1 TClass }
<0> "data"                      { act_token1 TData }
<0> "default"                   { act_token1 TDefault }
<0> "deriving"                  { act_token1 TDeriving }
<0> "do"                        { act_layout_keyword TDo }
<0> "else"                      { act_token1 TElse }
<0> "hiding"                    { act_token1 THiding }
<0> "foreign"                   { act_token1 TForeign }
<0> "if"                        { act_token1 TIf }
<0> "import"                    { act_token1 TImport }
<0> "in"                        { act_token1 TIn }
<0> "infix"                     { act_token1 TInfix }
<0> "infixl"                    { act_token1 TInfixl }
<0> "infixr"                    { act_token1 TInfixr }
<0> "instance"                  { act_token1 TInstance }
<0> "let"                       { act_layout_keyword TLet }
<0> "module"                    { act_token1 TModule }
<0> "newtype"                   { act_token1 TNewtype }
<0> "of"                        { act_layout_keyword TOf }
<0> "qualified"                 { act_token1 TQualified }
<0> "then"                      { act_token1 TThen }
<0> "type"                      { act_token1 TType }
<0> "where"                     { act_layout_keyword TWhere }
<0> "_"                         { act_token1 TUnderscore }

-- reserved op
<0> ".."                        { act_token1 TDotdot }
<0> ":"                         { act_token1 TColon }
<0> "::"                        { act_token1 TDColon }
<0> "="                         { act_token1 TEqual }
<0> "\\"                        { act_token1 TLam }
<0> "|"                         { act_token1 TVBar }
<0> "<-"                        { act_token1 TLArrow }
<0> "->"                        { act_token1 TRArrow }
<0> "@"                         { act_token1 TAt }
<0> "~"                         { act_token1 TTilde }
<0> "=>"                        { act_token1 TDArrow }
<0> "-"                         { act_token1 TMinus }
<0> "!"                         { act_token1 TBang }

<0> @varid                      { act_token2 TVarid }
<0> @conid                      { act_token2 TConid }
<0> @varsym                     { act_token2 TVarsym }
<0> @consym                     { act_token2 TConsym }

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
           | TAs AlexPosn
           | TCase AlexPosn
           | TClass AlexPosn
           | TData AlexPosn
           | TDefault AlexPosn
           | TDeriving AlexPosn
           | TDo AlexPosn
           | TElse AlexPosn
           | THiding AlexPosn
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
           | TQualified AlexPosn
           | TThen AlexPosn
           | TType AlexPosn
           | TWhere AlexPosn
           | TUnderscore AlexPosn
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
           | TMinus AlexPosn
           | TBang AlexPosn
           -- varid, conid, varsym, consym etc.
           | TVarid String AlexPosn
           | TConid String AlexPosn
           | TVarsym String AlexPosn
           | TConsym String AlexPosn
           | TQVarid String AlexPosn
           | TQConid String AlexPosn
           | TQVarsym String AlexPosn
           | TQConsym String AlexPosn
           -- Literals
           | TInteger Integer AlexPosn
           | TFloat Float AlexPosn
           | Eof
             deriving (Show)

data AlexUserState = AlexUserState { bof :: Bool
                                   , morrow :: Bool
                                   , layout_ctx :: [Int]
                                   , pending_toks :: [Token]
                                   , str :: String
                                   , comment_depth :: Int
                                   , last_char :: Char
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { bof = True
                                  , morrow = False
                                  , layout_ctx = []
                                  , pending_toks = []
                                  , str = ""
                                  , comment_depth = 0
                                  , last_char = '\0'
                                  }

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
act_nested_comment (_, _, _, s) _ = do
  c' <- getLastChar
  setLastChar c
  case (c':c:[]) of
    "{-" -> incDepth
    "-}" -> decDepth
    _ -> nop
  alexMonadScan
  where
    c = head s
    incDepth :: Alex ()
    incDepth = do d <- getCommentDepth; setCommentDepth (d + 1)
    decDepth :: Alex ()
    decDepth = do d <- getCommentDepth
                  setCommentDepth (d - 1)
                  alexSetStartCode $ if d > 1 then comment else 0

act_ocurly :: AlexAction Token
act_ocurly (pos, _, _, _) _ = do
  unFlags
  pushCtx 0
  return $ TOCurly pos

act_ccurly :: AlexAction Token
act_ccurly (pos, _, _, _) _ = do
  unFlags
  popCtx
  return $ TCCurly pos

act_layout_keyword :: (AlexPosn -> Token) -> AlexAction Token
act_layout_keyword maker (pos, _, _, _) _ = do setMorrow True; return tok
  where
    tok = maker pos

act_token1 :: (AlexPosn -> Token) -> AlexAction Token
act_token1 maker (pos@(AlexPn _ _ n), _, _, _) _ = do
  morr <- getMorrow
  beg <- getBof
  let lcond = if beg then
                case tok of
                  TModule _ -> False
                  _ -> True
              else
                morr
  unFlags
  m <- peepCtx
  if lcond then
    if n > m then
      do pushCtx n; appendToken tok; appendToken (TVOCurly pos)
    else
      do appendToken (TVOCurly pos); appendToken (TVCCurly pos); layout
  else
    layout
  alexMonadScan
 where
  tok = maker pos
  layout :: Alex ()
  layout = do
    m' <- peepCtx
    if n < m' then
      do popCtx; appendToken $ TVCCurly pos; layout
    else
      if n == m' then
        do appendToken $ TSemi pos; appendToken tok
      else
        appendToken tok

act_token2 :: (String -> AlexPosn -> Token) -> AlexAction Token
act_token2 maker inp@(_, _, _, s) len = 
  act_token1 (maker $ take len s) inp len

act_integer :: AlexAction Token
act_integer (pos, _, _, s) len = return (TInteger i pos)
  where
    i = read $ take len s  

act_float :: AlexAction Token
act_float (pos, _, _, s) len = return (TFloat x pos)
  where
    x = read $ take len s  
}
