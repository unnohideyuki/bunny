{
module Main where
import Lexer
import Absyn
}

%name parser
%error { parseError }
%lexer { lexwrap } { Eof }
%monad { Alex }
%tokentype { Token }

%token
'('        { TOParen     $$ }
')'        { TCParen     $$ }
','        { TComma      $$ }
';'        { TSemi       $$ }
'['        { TOBrack     $$ }
']'        { TCBrack     $$ }
'`'        { TBackquote  $$ }
'{'        { TOCurly     $$ }
'}'        { TCCurly     $$ }
vocurly    { TVOCurly    $$ }
vccurly    { TVCCurly    $$ }
'case'     { TCase       $$ }
'class'    { TClass      $$ }
'data'     { TData       $$ }
default    { TDefault    $$ }
deriving   { TDeriving   $$ }
do         { TDo         $$ }
else       { TElse       $$ }
foreign    { TForeign    $$ }
if         { TIf         $$ }
import     { TImport     $$ }
in         { TIn         $$ }
infix      { TInfix      $$ }
infixl     { TInfixl     $$ }
infixr     { TInfixr     $$ }
instance   { TInstance   $$ }
let        { TLet        $$ }
'module'   { TModule     $$ }
newtype    { TNewtype    $$ }
of         { TOf         $$ }
then       { TThen       $$ }
type       { TType       $$ }
'where'    { TWhere      $$ }
'_'        { TUnderscore $$ }
'as'            { TAs         $$ }
'hiding'        { THiding     $$ }
'qualified'     { TQualified  $$ }
'safe'          { TSafe       $$ }
'unsafe'        { TUnsafe     $$ }
dotdot     { TDotdot     $$ }
':'        { TColon      $$ }
dcolon     { TDColon     $$ }
'='        { TEqual      $$ }
'\\'       { TLam        $$ }
'|'        { TVBar       $$ }
larr       { TLArrow     $$ }
rarr       { TRArrow     $$ }
'@'             { TAt         $$ }
'~'             { TTilde      $$ }
darr            { TDArrow     $$ }
'-'             { TMinus      $$ }
'!'             { TBang       $$ }
tvarid          { TVarid      $$ }
tconid          { TConid      $$ }
tvarsym         { TVarsym     $$ }
tconsym         { TConsym     $$ }
tqvarid         { TQVarid     $$ }
tqconid         { TQConid     $$ }
tqvarsym        { TQVarsym    $$ }
tqconsym        { TQConsym    $$ }
tlitint         { TInteger    $$ }
tlitfloat       { TFloat      $$ }
tlitstr         { TString     $$ }
tlitchar        { TChar       $$ }

%%
module: 'module' modid 'where'  { mkModule $2 }

varid: tvarid                   { mkName $1 }
  |    'as'                     { mkName ("as", $1) }
  |    'hiding'                 { mkName ("hiding", $1) }
  |    'qualified'              { mkName ("qualified", $1) }
  |    'safe'                   { mkName ("safe", $1) }
  |    'unsafe'                 { mkName ("unsafe", $1) }

conid: tconid                   { mkName $1 }

varsym: tvarsym                 { mkName $1 }
  |     '-'                     { mkName ("-", $1) }
  |     '!'                     { mkName ("!", $1) }

qvarid: tqvarid                 { mkName $1 }
qconid: tqconid                 { mkName $1 }

modid:  qconid                  { $1 }
  |     conid                   { $1 }





{
extrPos :: AlexPosn -> Pos
extrPos (AlexPn _ line col) = (line, col)

extrQual qual name =
  case span (/= '.') name of
    (_, "")      -> (qual, name)
    (q, ('.':n)) -> extrQual (qual ++ q ++ ".") n
    (q, n)       -> extrQual (qual ++ q ++ ".") n

mkName (s, pos) = Name { name_body = body
                       , name_qual = Just qual
                       , name_pos  = extrPos pos }
  where
    (qual, body) = extrQual "" s

mkModule modid = Module modid []

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser

main :: IO ()
main = getContents >>= print . parse
}
