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

-- reserved id 
<0> "as" { skip }
<0> "case" { skip }
<0> "class" { skip }
<0> "data" { skip }
<0> "default" { skip }
<0> "deriving" { skip }
<0> "do" { skip }
<0> "else" { skip }
<0> "hiding" { skip }
<0> "foreign" { skip }
<0> "if" { skip }
<0> "import" { skip }
<0> "in" { skip }
<0> "infix" { skip }
<0> "infixl" { skip }
<0> "infixr" { skip }
<0> "instance" { skip }
<0> "let" { skip }
<0> "module" { skip }
<0> "newtype" { skip }
<0> "of" { skip }
<0> "qualified" { skip }
<0> "then" { skip }
<0> "type" { skip }
<0> "where" { skip }
<0> "_" { skip -- underscore}

-- reserved op
<0> ".." { skip -- dotdot}
<0> ":" { skip -- colon}
<0> "::" { skip -- dcolon}
<0> "=" { skip -- equal}
<0> "\\" { skip -- lam}
<0> "|" { skip -- vbar}
<0> "<-" { skip -- larrow}
<0> "->" { skip -- rarrow}
<0> "@" { skip  -- at}
<0> "~" { skip -- tilde}
<0> "=>" { skip -- darrow}

-- symbol
<0> "-" { skip -- minus}
<0> "!" { skip -- bang}

<0> @varid { skip }
<0> @conid { skip }
<0> @varsym { skip }
<0> @consym { skip }

<0> @qual @varid              { skip }
<0> @qual @conid              { skip }
<0> @qual @varsym             { skip }
<0> @qual @consym             { skip }

<0> @integer {skip}
<0> @float {skip}

{
data Token = TOBrace AlexPosn
           | TCBrace AlexPosn
           | VOBrace AlexPosn
           | VCBrace AlexPosn
           | TSemi AlexPosn
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

act_obrace :: AlexAction Token
act_obrace (pos, _, _, _) _ = do
  unFlags
  pushCtx 0
  return $ TOBrace pos

act_cbrace :: AlexAction Token
act_cbrace (pos, _, _, _) _ = do
  unFlags
  popCtx
  return $ TCBrace pos

act_layout_keyword :: AlexAction Token
act_layout_keyword (pos, _, _, s) len = do setMorrow True; return tok
  where
    tok = case take len s of
      "let" -> TLet pos
      "where" -> TWhere pos
      "do" -> TDo pos
      "of" -> TOf pos
      _ -> undefined

act_layout :: (AlexPosn -> Token) -> AlexAction Token
act_layout maker (pos@(AlexPn _ _ n), _, _, _) _ = do
  morr <- getMorrow
  unFlags
  m <- peepCtx
  if morr then
    if n > m then
      do pushCtx n; appendToken tok; appendToken (VOBrace pos)
    else
      do appendToken (VOBrace pos); appendToken (VCBrace pos)
  else
    nop
  layout
  alexMonadScan
 where
  tok = maker pos
  layout :: Alex ()
  layout = do
    m' <- peepCtx
    if n < m' then
      do popCtx; appendToken $ VCBrace pos; layout
    else
      if n == m' then
        appendToken $ TSemi pos
      else
        return ()
}
