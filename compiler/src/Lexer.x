{
module Lexer where
}
%wrapper "monadUserState"

tokens :-

{
data Token = Token (String, AlexPosn)
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
alexInitUserState = AlexUserState { bof = True,
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
getPendingToks = Alex $\st -> Right (st, pending_toks $ alex_ust st)

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

nop :: Monad m => m ()
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

pushTokens :: [Token] -> Alex ()
pushToken ts = Alex $ \st ->
  let
    ust = alex_ust st
    toks = pending_toks ust
    ust' = ust{pending_toks=ts ++ toks}
    inp = alex_inp st
    inp' = take (length ts) $ repeat ' ' ++ inp
    st' = st{alex_inp=inp', alex_ust=ust'}
  in
   Right (st', ())

moveColumn :: Int -> Alex ()
moveColumn x = Alex $ \st ->
  let
    (AlexPn abs line col) = alex_pos st
  in
   Right (st{alex_pos=AlexPn abs line (col + x)}, ())

pushCtx :: Int -> Alex ()
pushCtx m = do
  ms <- getLayoutCtx
  setLayoutCtx (m:ms)

popCtx :: Alex Int
popCtx = do
  ms <- getLayoutCtx
  case ms of
    [] -> Left "Cannot pop from empty layout context."
    (m:ms') -> do
      setLayoutCtx ms'
      return m

peepCtx :: Alex Int
peepCtx = do
  ms <- getLayoutCtx
  case ms of
    [] -> Left "Cannot pop from empty layout context."
    (m:_) -> return m

-- Token Actions

act_white_space :: AlexAction result
act_white_spece = do
  toks = getPendingToks
  case toks of
    (t:ts) -> do setPendingToks ts
                 moveColumn (-1)
                 return t
    [] -> alexMonadScan

}
