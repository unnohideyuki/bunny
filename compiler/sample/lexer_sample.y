{
module Main where
import Lexer
}

%name parser
%error { parseError }
%lexer { lexwrap } { Eof }
%monad { Alex }
%tokentype { Token }

%token
"("        { TOParen     $$ }
")"        { TCParen     $$ }
","        { TComma      $$ }
";"        { TSemi       $$ }
"["        { TOBrack     $$ }
"]"        { TCBrack     $$ }
"`"        { TBackquote  $$ }
"{"        { TOCurly     $$ }
"}"        { TCCurly     $$ }
vocurly    { TVOCurly    $$ }
vccurly    { TVCCurly    $$ }
case       { TCase       $$ }
class      { TClass      $$ }
data       { TData       $$ }
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
module     { TModule     $$ }
newtype    { TNewtype    $$ }
of         { TOf         $$ }
then       { TThen       $$ }
type       { TType       $$ }
where      { TWhere      $$ }
"_"        { TUnderscore $$ }
as         { TAs         $$ }
hiding     { THiding     $$ }
qualified  { TQualified  $$ }
safe       { TSafe       $$ }
unsafe     { TUnsafe     $$ }
dotdot     { TDotdot     $$ }
":"        { TColon      $$ }
dcolon     { TDColon     $$ }
"="        { TEqual      $$ }
"\\"       { TLam        $$ }
"|"        { TVBar       $$ }
larr       { TLArrow     $$ }
rarr       { TRArrow     $$ }
"@"        { TAt         $$ }
"~"        { TTilde      $$ }
darr       { TDArrow     $$ }
"-"        { TMinus      $$ }
"!"        { TBang       $$ }
varid      { TVarid   $$ }
conid      { TConid   $$ }
varsym     { TVarsym  $$ }
consym     { TConsym  $$ }
qvarid     { TQVarid  $$ }
qconid     { TQConid  $$ }
qvarsym    { TQVarsym $$ }
qconsym    { TQConsym $$ }
litint     { TInteger $$ }
litfloat   { TFloat   $$ }
litstr     { TString  $$ }
litchar    { TChar $$ }

%%
tokens:  token                  { [$1] }
  | token tokens                { [$1] ++ $2 }

token:  "("        { ("(", $1) }
  |     ")"        { (")", $1) }
  |     vocurly    { ("{", $1) }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser

main :: IO ()
main = getContents >>= print . parse 

}