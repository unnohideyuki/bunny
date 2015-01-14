{
module Lexer where
}
%wrapper "monadUserState"

tokens :-

<0> $white                      { act_white_space }

<0,comment> "{-"                { act_open_comment }
<comment> $white+               { skip }
<comment> .                     { act_ncomment }

{
data Token = TOBrace AlexPosn
           | TCBrace AlexPosn
           | VOBrace AlexPosn
           | VCBrace AlexPosn
           | TSemi AlexPosn
           | TLet AlexPosn
           | TWhere AlexPosn
           | TDo AlexPosn
           | TOf AlexPosn
           | Eof
             deriving (Show)

data AlexUserState = AlexUserState { bof :: Bool
                                   , morrow :: Bool
                                   , layout_ctx :: [Int]
                                   , pending_toks :: [Token]
                                   , str :: String
                                   , comment_depth :: Int
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { bof = True
                                  , morrow = False
                                  , layout_ctx = []
                                  , pending_toks = []
                                  , str = ""
                                  , comment_depth = 0
                                  }

-- State Utilities

getBof :: Alex Bool
getBof = Alex $ \st -> Right (st, bof $ alex_ust st)

setBof :: Bool -> Alex ()
setBof b = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{bof=b}
  in
   Right (st{alex_ust=ust'}, ())

getMorrow :: Alex Bool
getMorrow = Alex $ \st -> Right (st, morrow $ alex_ust st)

setMorrow :: Bool -> Alex ()
setMorrow b = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{morrow=b}
  in
   Right (st{alex_ust=ust'}, ())

unFlags :: Alex ()
unFlags = do setBof False; setMorrow False

getLayoutCtx :: Alex [Int]
getLayoutCtx = Alex $ \st -> Right (st, layout_ctx $ alex_ust st)

setLayoutCtx :: [Int] -> Alex ()
setLayoutCtx cs = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{layout_ctx=cs}
  in
   Right (st{alex_ust=ust'}, ())

getPendingToks :: Alex [Token]
getPendingToks = Alex $ \st -> Right (st, pending_toks $ alex_ust st)

setPendingToks :: [Token] -> Alex ()
setPendingToks ts = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{pending_toks=ts}
  in
   Right (st{alex_ust=ust'}, ())

getStr :: Alex String
getStr = Alex $ \st -> Right (st, str $ alex_ust st)

setStr :: String -> Alex ()
setStr s = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{str=s}
  in
   Right (st{alex_ust=ust'}, ())

getCommentDepth :: Alex Int
getCommentDepth = Alex $ \st -> Right (st, comment_depth $ alex_ust st)

setCommentDepth :: Int -> Alex ()
setCommentDepth d = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{comment_depth=d}
  in
   Right (st{alex_ust=ust'}, ())

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

act_ncomment :: AlexAction Token
act_ncomment (pos, _, _, _) len = undefined

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
