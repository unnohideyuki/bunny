{-# OPTIONS_GHC -w #-}
module Main where
import Lexer
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

action_0 (6) = happyShift action_3
action_0 (7) = happyShift action_4
action_0 (15) = happyShift action_5
action_0 (4) = happyGoto action_6
action_0 (5) = happyGoto action_7
action_0 _ = happyFail

action_1 (6) = happyShift action_3
action_1 (7) = happyShift action_4
action_1 (15) = happyShift action_5
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 _ = happyFail

action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 (70) = happyAccept
action_6 _ = happyFail

action_7 (6) = happyShift action_3
action_7 (7) = happyShift action_4
action_7 (15) = happyShift action_5
action_7 (4) = happyGoto action_8
action_7 (5) = happyGoto action_7
action_7 _ = happyReduce_1

action_8 _ = happyReduce_2

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1] ++ happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyTerminal (TOParen     happy_var_1))
	 =  HappyAbsSyn5
		 (("(", happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyTerminal (TCParen     happy_var_1))
	 =  HappyAbsSyn5
		 ((")", happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyTerminal (TVOCurly    happy_var_1))
	 =  HappyAbsSyn5
		 (("{", happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Eof -> action 70 70 tk (HappyState action) sts stk;
	TOParen     happy_dollar_dollar -> cont 6;
	TCParen     happy_dollar_dollar -> cont 7;
	TComma      happy_dollar_dollar -> cont 8;
	TSemi       happy_dollar_dollar -> cont 9;
	TOBrack     happy_dollar_dollar -> cont 10;
	TCBrack     happy_dollar_dollar -> cont 11;
	TBackquote  happy_dollar_dollar -> cont 12;
	TOCurly     happy_dollar_dollar -> cont 13;
	TCCurly     happy_dollar_dollar -> cont 14;
	TVOCurly    happy_dollar_dollar -> cont 15;
	TVCCurly    happy_dollar_dollar -> cont 16;
	TCase       happy_dollar_dollar -> cont 17;
	TClass      happy_dollar_dollar -> cont 18;
	TData       happy_dollar_dollar -> cont 19;
	TDefault    happy_dollar_dollar -> cont 20;
	TDeriving   happy_dollar_dollar -> cont 21;
	TDo         happy_dollar_dollar -> cont 22;
	TElse       happy_dollar_dollar -> cont 23;
	TForeign    happy_dollar_dollar -> cont 24;
	TIf         happy_dollar_dollar -> cont 25;
	TImport     happy_dollar_dollar -> cont 26;
	TIn         happy_dollar_dollar -> cont 27;
	TInfix      happy_dollar_dollar -> cont 28;
	TInfixl     happy_dollar_dollar -> cont 29;
	TInfixr     happy_dollar_dollar -> cont 30;
	TInstance   happy_dollar_dollar -> cont 31;
	TLet        happy_dollar_dollar -> cont 32;
	TModule     happy_dollar_dollar -> cont 33;
	TNewtype    happy_dollar_dollar -> cont 34;
	TOf         happy_dollar_dollar -> cont 35;
	TThen       happy_dollar_dollar -> cont 36;
	TType       happy_dollar_dollar -> cont 37;
	TWhere      happy_dollar_dollar -> cont 38;
	TUnderscore happy_dollar_dollar -> cont 39;
	TAs         happy_dollar_dollar -> cont 40;
	THiding     happy_dollar_dollar -> cont 41;
	TQualified  happy_dollar_dollar -> cont 42;
	TSafe       happy_dollar_dollar -> cont 43;
	TUnsafe     happy_dollar_dollar -> cont 44;
	TDotdot     happy_dollar_dollar -> cont 45;
	TColon      happy_dollar_dollar -> cont 46;
	TDColon     happy_dollar_dollar -> cont 47;
	TEqual      happy_dollar_dollar -> cont 48;
	TLam        happy_dollar_dollar -> cont 49;
	TVBar       happy_dollar_dollar -> cont 50;
	TLArrow     happy_dollar_dollar -> cont 51;
	TRArrow     happy_dollar_dollar -> cont 52;
	TAt         happy_dollar_dollar -> cont 53;
	TTilde      happy_dollar_dollar -> cont 54;
	TDArrow     happy_dollar_dollar -> cont 55;
	TMinus      happy_dollar_dollar -> cont 56;
	TBang       happy_dollar_dollar -> cont 57;
	TVarid   happy_dollar_dollar -> cont 58;
	TConid   happy_dollar_dollar -> cont 59;
	TVarsym  happy_dollar_dollar -> cont 60;
	TConsym  happy_dollar_dollar -> cont 61;
	TQVarid  happy_dollar_dollar -> cont 62;
	TQConid  happy_dollar_dollar -> cont 63;
	TQVarsym happy_dollar_dollar -> cont 64;
	TQConsym happy_dollar_dollar -> cont 65;
	TInteger happy_dollar_dollar -> cont 66;
	TFloat   happy_dollar_dollar -> cont 67;
	TString  happy_dollar_dollar -> cont 68;
	TChar happy_dollar_dollar -> cont 69;
	_ -> happyError' tk
	})

happyError_ 70 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (>>=)
happyReturn :: () => a -> Alex a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> Alex a
happyError' tk = parseError tk

parser = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser

main :: IO ()
main = getContents >>= print . parse
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 5 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
