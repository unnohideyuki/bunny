{
module Main where
import Lexer
import Absyn
}

%name      parser
%error     { parseError }
%lexer     { lexwrap }{ Eof }
%monad     { Alex }
%tokentype { Token }

%token
'('         { TOParen     $$ }
')'         { TCParen     $$ }
','         { TComma      $$ }
';'         { TSemi       $$ }
'['         { TOBrack     $$ }
']'         { TCBrack     $$ }
'`'         { TBackquote  $$ }
'{'         { TOCurly     $$ }
'}'         { TCCurly     $$ }
vocurly     { TVOCurly    $$ }
vccurly     { TVCCurly    $$ }
'case'      { TCase       $$ }
'class'     { TClass      $$ }
'data'      { TData       $$ }
'default'   { TDefault    $$ }
'deriving'  { TDeriving   $$ }
'do'        { TDo         $$ }
'else'      { TElse       $$ }
'foreign'   { TForeign    $$ }
'if'        { TIf         $$ }
'import'    { TImport     $$ }
'in'        { TIn         $$ }
'infix'     { TInfix      $$ }
'infixl'    { TInfixl     $$ }
'infixr'    { TInfixr     $$ }
'instance'  { TInstance   $$ }
'let'       { TLet        $$ }
'module'    { TModule     $$ }
'newtype'   { TNewtype    $$ }
'of'        { TOf         $$ }
'then'      { TThen       $$ }
'type'      { TType       $$ }
'where'     { TWhere      $$ }
'_'         { TUnderscore $$ }
'as'        { TAs         $$ }
'hiding'    { THiding     $$ }
'qualified' { TQualified  $$ }
'safe'      { TSafe       $$ }
'unsafe'    { TUnsafe     $$ }
'..'        { TDotdot     $$ }
':'         { TColon      $$ }
'::'        { TDColon     $$ }
'='         { TEqual      $$ }
'\\'        { TLam        $$ }
'|'         { TVBar       $$ }
'<-'        { TLArrow     $$ }
'->'        { TRArrow     $$ }
'@'         { TAt         $$ }
'~'         { TTilde      $$ }
'=>'        { TDArrow     $$ }
'-'         { TMinus      $$ }
'!'         { TBang       $$ }
tvarid      { TVarid      $$ }
tconid      { TConid      $$ }
tvarsym     { TVarsym     $$ }
tconsym     { TConsym     $$ }
tqvarid     { TQVarid     $$ }
tqconid     { TQConid     $$ }
tqvarsym    { TQVarsym    $$ }
tqconsym    { TQConsym    $$ }
tlitint     { TInteger    $$ }
tlitfloat   { TFloat      $$ }
tlitstr     { TString     $$ }
tlitchar    { TChar       $$ }

%%
module: 'module' modid exports 'where' body     { mkModule $2 }

exports: '(' seq_exports     ')'                {}
  |      '(' seq_exports ',' ')'                {}
  |      '('                 ')'                {}
  |      {- empty -}                            {}

seq_exports:  seq_exports ',' export            {}

export: 'module' modid                          {}

body:    {- empty -}                            {}

decls:                                          {}
guards:                                         {}
exp:                                            {}



qual: pat '<-' exp                              {}
  |   'let' decls                               {}
  |   exp                                       {}

alts: alts ';' alt                              {}
  |   alt                                       {}

alt: pat '->' exp                               {}
  |  pat '->' exp 'where' decls                 {}
  |  pat gdpat                                  {}
  |  pat gdpat 'where' decls                    {}

gdpat: guards '->' exp                          {}
  |    guards '->' exp gdpat                    {}

stmts: seq_stmt exp ';'                         {}
  |    seq_stmt exp                             {}

seq_stmt: seq_stmt stmt                         {}
  |       {- empty -}                           {}

stmt: exp ';'                                   {}
  |   pat '<-' exp ';'                          {}
  |   'let' decls ';'                           {}
  |   ';'                                       {}

fbind: qvar '=' exp                             {}

pat: lpat qconop pat                            {}
  |  lpat                                       {}

lpat: apat                                      {}
  |   '-' integer                               {}
  |   '-' float                                 {}
  |   gcon seq_apat                             {}

seq_apat: seq_apat apat                         {}
  |       {- empty -}                           {}

apat: var                                       {}
  |   var '@' apat                              {}
  |   gcon                                      {}
  |   qcon '{' seq_fpat '}'                     {}
  |   literal                                   {}
  |   '_'                                       {}
  |   '(' pat ')'                               {}
  |   '(' seq1_pat ',' pat ')'                  {}
  |   '[' seq1_pat ']'                          {}
  |   '~' apat                                  {}

seq1_pat: seq1_pat pat                          {}
  |       pat                                   {}

seq_fpat: seq_fpat fpat                         {}
  |       {- empty -}                           {}

fpat: qvar '=' pat                              {}

gcon: '(' ')'                                   {}
  |   '[' ']'                                   {}
  |   '(' ',' seq_commas ')'                    {}
  |   qcon                                      {}

seq_commas: seq_commas ','                      {}
  |         {- empty -}                         {}

var: varid                                      {}
  |  '(' varsym ')'                             {}

qvar: qvarid                                    {}
  |  '(' qvarsym ')'                            {}
  |   var                                       {}

con: conid                                      {}
  |  '(' consym ')'                             {}

qcon : qconid                                   {}
  |    '(' gconsym ')'                          {}
  |    con                                      {}

varop : varsym                                  {}
  |     '`' varid '`'                           {}

qvarop: qvarsym                                 {}
  |     '`' qvarid '`'                          {}
  |     varop                                   {}

conop: consym                                   {}
  |    '`' conid '`'                            {}

qconop: gconsym                                 {}
  |     '`' qconid '`'                          {}
  |     conop                                   {}

op: varop                                       {}
  | conop                                       {}

qop: qvarop                                     {}
  |  qconop                                     {}

-- qop<->
qop_: varsym_                                   {}
  |   '`' varid '`'                             {}
  |   qvarsym                                   {}
  |   '`' qvarid '`'                            {}
  |   qconop                                    {}

gconsym: ':'                                    {}
  | qconsym                                     {}


modid:  qconid                  { $1 }
  |     conid                   { $1 }

varid: tvarid                                   { mkName $1 }
  |    'as'                                     { mkName ("as", $1) }
  |    'hiding'                                 { mkName ("hiding", $1) }
  |    'qualified'                              { mkName ("qualified", $1) }
  |    'safe'                                   { mkName ("safe", $1) }
  |    'unsafe'                                 { mkName ("unsafe", $1) }

conid: tconid                                   { mkName $1 }

varsym: tvarsym                                 { mkName $1 }
  |     '-'                                     { mkName ("-", $1) }
  |     '!'                                     { mkName ("!", $1) }

varsym_: tvarsym                                {}
  |      '!'                                    {}

consym: tconsym                                 {}

qvarid: tqvarid                                 { mkName $1 }
qconid: tqconid                                 { mkName $1 }
qvarsym: tqvarsym                               {}
qconsym: tqconsym                               {}

integer: tlitint                                {}
float:   tlitfloat                              {}
char:    tlitchar                               {}
string:  tlitstr                                {}

literal: integer                                {}
  |      float                                  {}
  |      char                                   {}
  |      string                                 {}
{
extrPos :: AlexPosn -> Pos
extrPos (AlexPn _ line col) = (line, col)

extrQual qual name =
  case span (/= '.') name of
    (_, "")      -> (qual, name)
    (q, ('.':n)) -> extrQual (qual ++ q ++ ".") n
    (q, n)       -> extrQual (qual ++ q ++ ".") n

mkName (s, pos) = Name { name_body = body
                       , name_qual = qual
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
