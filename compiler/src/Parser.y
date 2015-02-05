{
module Parser where
import Lexer
import Absyn
import ParserHelper
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
TVARID      { TVarid      $$ }
TCONID      { TConid      $$ }
TVARSYM     { TVarsym     $$ }
TCONSYM     { TConsym     $$ }
TQVARID     { TQVarid     $$ }
TQCONID     { TQConid     $$ }
TQVARSYM    { TQVarsym    $$ }
TQCONSYM    { TQConsym    $$ }
TLITINT     { TInteger    $$ }
TLITFLOAT   { TFloat      $$ }
TLITSTR     { TString     $$ }
TLITCHAR    { TChar       $$ }

%%
-- Module Header --------------------------------------------------------------
module: 'module' modid exports_opt 'where' body { mkModule (Just $2) }
      | body                                    { mkModule Nothing }

body: '{'     top '}'                           { $2 }
    | vocurly top close                         { $2 }

top: impdecls                                   {}
   | impdecls ';' topdecls                      {}
   | topdecls                                   {}

-- Export List ----------------------------------------------------------------
exports_opt: '(' exportlist ')'                 {}
           | {- empty -}                        {}

exportlist: exportlist1                         {}
          | exportlist1 ','                     {}
          | ','                                 {}
          | {- empty -}                         {}

exportlist1: export ',' exportlist1             {}
           | export                             {}

export: qvar                                    {}
      | oqtycon                                 {}
      | oqtycon '(' '..' ')'                    {}
      | oqtycon '(' ')'                         {}
      | oqtycon '(' qcnames ')'                 {}
      | 'module' modid                          {}

qcnames: qcnames ',' qcname                     {}
       | qcname                                 {}

qcname: qvar                                    { $1 }
      | qcon                                    { $1 }

-- Import Declarations --------------------------------------------------------
impdecls: impdecls ';' impdecl                  {}
        | impdecls ';'                          {}
        | impdecl                               {}
        | {- empty -}                           {}

impdecl: 'import' qual_opt modid as_opt impspec_opt
                                                {}

qual_opt: 'qualified'                           {}
        | {- empty -}                           {}

as_opt: 'as' modid                              {}
      | {- empty -}                             {}

impspec_opt: impspec                            {}
           | {- empty -}                        {}

impspec: '(' exportlist ')'                     {}
       | 'hiding' '(' exportlist ')'            {}

-- Fixity Declarations --------------------------------------------------------
prec: {- empty -}                               { {- 9 -} }
    | TLITINT                                   {}

fixity: 'infixl'                                {}
      | 'infixr'                                {}
      | 'infix'                                 {}

ops: ops ',' op                                 {}
   | op                                         {}

-- Top-Level Declarations -----------------------------------------------------
topdecls: topdecls ';' topdecl                  {}
        | topdecls ';'                          {}
        | topdecl                               {}

topdecl: cl_decl                                {}
       | ty_decl                                {}
       | 'instance' inst_type where_inst        {}
       | 'default' '(' comma_types0 ')'         {}
       | 'foreign' fdecl                        {}
       | decl                                   {}

cl_decl: 'class' tycl_hdr fds where_cls         {}

ty_decl: 'type' type '=' type                   {}
       | data_or_newtype tycl_hdr constrs deriving
                                                {}
data_or_newtype: 'data'                         {}
               | 'newtype'                      {}

tycl_hdr: context '=>' type                     {}
        | type                                  {}

-- Class body
decl_cls: decl                                  {}

decls_cls : decls_cls ';' decl_cls              {}
          | decls_cls ';'                       {}
          | decl_cls                            {}
          | {- empty -}                         {}

decllist_cls
  : '{'     decls_cls '}'                       {}
  | vocurly decls_cls close                     {}

where_cls: 'where' decllist_cls                 {}
         | {- empty -}                          {}

-- Instance body
decl_inst: decl                                 {}

decls_inst: decls_inst ';' decl_inst            {}
          | decls_inst ';'                      {}
          | decl_inst                           {}
          | {- empty -}                         {}

decllist_inst
  : '{'     decls_inst '}'                      {}
  | vocurly decls_inst close                    {}

where_inst: 'where' decllist_inst               {}
          | {- empty -}                         {}

-- Declarations
decls: decls ';' decl                           {}
     | decls ';'                                {}
     | decl                                     {}
     | {- empty -}                              {}

decllist
  : '{'     decls '}'                           {}
  | vocurly decls close                         {}

-- Binding groups
binds: decllist                                 {}

wherebinds: 'where' binds                       {}
          | {- empty -}                         {}

-- Foreign import/export declarations -----------------------------------------
fdecl: 'import' callconv safety fspec           {}
     | 'import' callconv        fspec           {}

callconv: {- tbd -}                             {}

safety: 'unsafe'                                {}
      |     'safe'                              {}

fspec: TLITSTR var '::' sigtypedoc              {}
     |        var '::' sigtypedoc               {}

-- Type signatures ------------------------------------------------------------
sigtypedoc: context '=>' type                   {}
          | type                                {}

sig_vars: sig_vars ',' var                      {}
        | var                                   {}

-- There is a note for 'context' in the GHC's source.
context: btype                                  { $1 }

type: btype                                     { $1 }
    | btype '->' type                           { FunTy $1 $3 }

btype: btype atype                              { AppTy $1 $2 }
     | atype                                    { $1 }

atype: gtycon                                   { Tycon $1 }
     | tyvar                                    { Tyvar $1 }
     | '(' type ',' comma_types1 ')'            { TupleTy ($2:$4) }
     | '[' type ']'                             { ListTy $2 }
     | '(' type ')'                             { ParTy $2 }
     -- constructor sigs only
     | '!' atype                                { BangTy $2 }
     | '{' fielddecls '}'                       { undefined }

inst_type: type                                 { $1 }

inst_types1: inst_type                          { [$1] }
           | inst_type ',' inst_types1          { $1:$3 }

comma_types0: comma_types1                      { $1 }
            | {- empty -}                       { [] }

comma_types1: type                              { [$1] }
            | type ',' comma_types1             { $1:$3 }

fds: {- empty -}                                {}
   | '|' fds1                                   {}

fds1: fds1 ',' fd                               {}
    | fd                                        {}

fd: varids0 '->' varids0                        {}

varids0: {- empty -}                            {}
       | varids0 tyvar                          {}

-- Datatype declarations ------------------------------------------------------
constrs: '=' constrs1                           {}

constrs1: constrs1 '|' constr                   {}
        | constr                                {}

constr: constr_stuff                            {}

constr_stuff
 : btype                                        {}
 | btype conop btype                            {}

fielddecls: {- empty -}                         {}
          | fielddecls1                         {}

fielddecls1
 : fielddecl ',' fielddecls1                    {}
 | fielddecl                                    {}

fielddecl: sig_vars '::' type                   {}

deriving: 'deriving' qtycon                     { Just [Tycon $2] }
        | 'deriving' '(' ')'                    { Just [] }
        | 'deriving' '(' inst_types1 ')'        { Just $3 }
        | {- empty -}                           { Nothing }

-- Value definitions ----------------------------------------------------------
decl: sigdecl                                   {}
    | '!' aexp rhs                              {}
    | infixexp rhs                              {}

rhs: '=' exp wherebinds                         {}
   | gdrhs wherebinds                           {}

gdrhs: gdrhs gdrh                               {}
     | gdrh                                     {}

gdrh: '|' guardquals '=' exp                    {}

sigdecl
  : infixexp '::' sigtypedoc                    {}
  | var ',' sig_vars '::' sigtypedoc            {}
  | fixity prec ops                             {}

-- Expressions ----------------------------------------------------------------
exp: infixexp '::' sigtypedoc                   { ExpWithTySig $1 {- todo -} }
   | infixexp                                   { $1 }

infixexp: exp10                                 { $1 }
        | infixexp qop exp10                    { InfixExp $1 $2 $3 }

exp10
  : '\\' apat apats '->' exp                    { LamExp ($2:$3) $5 }
  | 'let' binds 'in' exp                        { undefined }
  | 'if' exp semi_opt 'then' exp semi_opt 'else' exp
                                                { IfExp $2 $5 $8 }
  | 'case' exp 'of' altslist                    { undefined }
  | '-' fexp                                    { UMinusExp $2 }
  | 'do' stmtlist                               { undefined }
  | fexp                                        { $1 }

semi_opt: ';'                                   { () }
        | {- empty -}                           { () }

fexp: fexp aexp                                 { FunAppExp $1 $2 }
    | aexp                                      { $1 }

aexp: qvar '@' aexp                             { AsPat $1 $3 }
    | '~' aexp                                  { LazyPat $2 }
    | aexp1                                     { $1 }

aexp1: aexp1 '{' fbinds '}'                     { RecordConOrUpdate $1 $3 }
     | aexp2                                    { $1 }

aexp2
  : qcname                                      { VarExp $1 }
  | literal                                     { LitExp $1 }
  | '(' texp ')'                                { undefined }
  | '(' tup_exprs ')'                           { undefined }
  | '[' list ']'                                { undefined }
  | '_'                                         { WildcardPat }

-- Tuple expressions ----------------------------------------------------------
texp: exp                                       {}
    | infixexp qop                              {}
    | qopm infixexp                             {}
    | exp '->' texp                             {}

tup_exprs: texp commas_tup_tail                 {}
         | commas tup_tail                      {}

commas_tup_tail: commas tup_tail                {}

tup_tail: texp commas_tup_tail                  {}
        | texp                                  {}
        | {- empty -}                           {}

-- List expressions -----------------------------------------------------------
list: texp                                      {}
    | lexps                                     {}
    | texp '..'                                 {}
    | texp ',' exp '..'                         {}
    | texp '..' exp                             {}
    | texp ',' exp '..' exp                     {}
    | texp '|' squals                           {}

lexps: lexps ',' texp                           {}
     | texp ',' texp                            {}

-- List Comprehensions --------------------------------------------------------
squals: squals ',' qual                         {}
      | qual                                    {}

-- Guards ---------------------------------------------------------------------
guardquals: guardquals1                         {}

guardquals1
  : guardquals1 ',' qual                        {}
  | qual                                        {}

-- Case alternatives ----------------------------------------------------------
altslist
  : '{'     alts '}'                            {}
  | vocurly alts close                          {}

alts: alts1                                     {}
    | ';' alts                                  {}

alts1: alts1 ';' alt                            {}
     | alts1 ';'                                {}
     | alt                                      {}

alt: pat alt_rhs                                {}

alt_rhs: ralt wherebinds                        {}

ralt: '->' exp                                  {}
    | gdpats                                    {}

gdpats: gdpats gdpat                            {}
      | gdpat                                   {}

gdpat: '|' guardquals '->' exp                  {}

pat: exp                                        {}
   | '!' aexp                                   {}

apat: aexp                                      { $1 }
    | '!' aexp                                  { undefined }

apats: apat apats                               { $1:$2 }
     | {- empty -}                              { [] }

-- Statement sequences --------------------------------------------------------
stmtlist
  : '{'     stmts '}'                           {}
  | vocurly stmts close                         {}

stmts: stmt stmts_help                          {}
     | ';' stmts                                {}
     | {- empty -}                              {}

stmts_help: ';' stmts                           {}
          | {- empty -}                         {}

stmt: qual                                      {}

qual: pat '<-' exp                              {}
    | exp                                       {}
    | 'let' binds                               {}

-- Record Field Updata/Construction -------------------------------------------
fbinds: fbinds1                                 { $1 }
      | {- empty -}                             { ([], False) }

fbinds1: fbind ',' fbinds1
                                          { case $3 of (fs, b) -> ($1 : fs,b) }
       | fbind                                  { ([$1], False) }
       | '..'                                   { ([], True) }

fbind: qvar '=' exp                             { mkRecField $1 undefined }

-- Data constructors ----------------------------------------------------------
qcon: qconid                                    { $1 }
    | '(' qconsym ')'                           { $2 }
    | sysdcon                                   { $1 }

sysdcon
  : '(' ')'                                     { mkName ("()", $1) }
  | '(' commas ')'                              { mkName ("(" ++ $2 ++ ")", $1) }
  | '[' ']'                                     { mkName ("[]", $1) }

conop: consym                                   { $1 }
     | '`' conid '`'                            { $2 }

qconop: qconsym                                 { $1 }
      | '`' qconid '`'                          { $2 }

-- Type constructors ----------------------------------------------------------
gtycon -- A "general" qualified tycon
  : oqtycon                                     { $1 }
  | '(' ')'                                     { mkName ("()", $1) }
  | '(' commas ')'                              { mkName ("(" ++ $2 ++ ")", $1) }
  | '(' '->' ')'                                { mkName ("(->)", $1) }
  | '[' ']'                                     { mkName ("[]", $1) }

oqtycon -- An "ordinary" qualified tycon
  : qtycon                                      { $1 }
  | '(' qtyconsym ')'                           { $2 }
  | '(' '~' ')'                                 { mkName ("~", $2) }

qtycon: TQCONID                                 { mkName $1 }
      | tycon                                   { $1 }

tycon: TCONID                                   { mkName $1 }

qtyconsym: TQCONSYM                             { mkName $1 }
         | tyconsym                             { $1 }

tyconsym: TCONSYM                               { mkName $1 }

-- Operators ------------------------------------------------------------------
op: varop                                       { $1 }
  | conop                                       { $1 }

varop : varsym                                  { $1 }
      | '`' varid '`'                           { $2 }

qop: qvarop                                     { $1 }
  |  qconop                                     { $1 }

qopm: qvaropm                                   { $1 }
    | qconop                                    { $1 }

qvarop: qvarsym                                 { $1 }
      | '`' qvarid '`'                          { $2 }

qvaropm: qvarsym_no_minus                       { $1 }
       | '`' qvarid '`'                         { $2 }

-- Type variables -------------------------------------------------------------
tyvar: tyvarid                                  { $1 }
     | '(' tyvarsym ')'                         { $2 }

tyvarid
  : TVARID                                      { mkName $1 }
  | 'as'                                        { mkName ("as", $1) }
  | 'hiding'                                    { mkName ("hiding", $1) }
  | 'qualified'                                 { mkName ("qualified", $1) }
  | 'safe'                                      { mkName ("safe", $1) }
  | 'unsafe'                                    { mkName ("unsafe", $1) }

tyvarsym: TVARSYM                               { mkName $1 }

-- Variables ------------------------------------------------------------------
var: varid                                      { $1 }
   | '(' varsym ')'                             { $2 }

qvar: qvarid                                    { $1 }
    | '(' varsym ')'                            { $2 }
    | '(' qvarsym1 ')'                          { $2 }

qvarid: varid                                   { $1 }
      | TQVARID                                 { mkName $1 }

varid
  : TVARID                                      { mkName $1 }
  | 'as'                                        { mkName ("as", $1) }
  | 'hiding'                                    { mkName ("hiding", $1) }
  | 'qualified'                                 { mkName ("qualified", $1) }
  | 'safe'                                      { mkName ("safe", $1) }
  | 'unsafe'                                    { mkName ("unsafe", $1) }

qvarsym: varsym                                 { $1 }
       | qvarsym1                               { $1 }

qvarsym_no_minus
  : varsym_no_minus                             { $1 }
  | qvarsym1                                    { $1 }

qvarsym1: TQVARSYM                              { mkName $1 }

varsym: varsym_no_minus                         { $1 }
      | '-'                                     { mkName ("-", $1) }

varsym_no_minus: TVARSYM                        { mkName $1 }
               | '!'                            { mkName ("!", $1) }

-- Data constructors ----------------------------------------------------------
qconid: conid                                   { $1 }
      | TQCONID                                 { mkName $1 }

conid: TCONID                                   { mkName $1 }

qconsym: consym                                 { $1 }
       | TQCONSYM                               { mkName $1 }

consym: TCONSYM                                 { mkName $1 }
      | ':'                                     { mkName (":", $1) }

-- Literals -------------------------------------------------------------------
literal: TLITCHAR                               { mkChar $1 }
  |      TLITSTR                                { mkString $1 }
  |      TLITINT                                { mkInteger $1 }
  |      TLITFLOAT                              { mkFloat $1 }

-- Layout ---------------------------------------------------------------------
close: vccurly                                  { () }
     | error                                    {% popCtx }

-- Misc -----------------------------------------------------------------------
commas: commas ','                              { ',' : $1 }
      | ','                                     { "," }

modid: TCONID                                   { mkName $1 }
     | TQCONID                                  { mkName $1 }
{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser
}
