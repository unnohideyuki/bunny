{
module Parser where
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
module: 'module' modid exports_opt 'where' body { Module (Just $2) $3 $5 }
      | body                                    { Module Nothing Nothing $1 }

body: '{'     top '}'                           { $2 }
    | vocurly top close                         { $2 }

top: impdecls                                   { ($1, []) }
   | impdecls ';' topdecls                      { ($1, $3) }
   | topdecls                                   { ([], $1) }

-- Export List ----------------------------------------------------------------
exports_opt: '(' exportlist ')'                 { Just $2 }
           | {- empty -}                        { Nothing }

exportlist: exportlist1                         { $1 }
          | exportlist1 ','                     { $1 }
          | ','                                 { [] }
          | {- empty -}                         { [] }

exportlist1: export ',' exportlist1             { $1:$3 }
           | export                             { [$1] }

export: qvar                                    { IEVar $1 }
      | oqtycon                                 { IEThingAbs $1 }
      | oqtycon '(' '..' ')'                    { IEThingAll $1 }
      | oqtycon '(' ')'                         { IEThingWith $1 [] }
      | oqtycon '(' qcnames ')'                 { IEThingWith $1 $3 }
      | 'module' modid                          { IEModuleContents $2 }

qcnames: qcnames ',' qcname                     { $3:$1 }
       | qcname                                 { [$1] }

qcname: qvar                                    { $1 }
      | qcon                                    { $1 }

-- Import Declarations --------------------------------------------------------
impdecls: impdecls ';' impdecl                  { $1 ++ [$3] }
        | impdecls ';'                          { $1 }
        | impdecl                               { [$1] }
        | {- empty -}                           { [] }

impdecl: 'import' qual_opt modid as_opt impspec_opt
                                                { ImportDecl $2 $3 $4 $5 }

qual_opt: 'qualified'                           { True }
        | {- empty -}                           { False }

as_opt: 'as' modid                              { Just $2 }
      | {- empty -}                             { Nothing }

impspec_opt: impspec                            { Just $1 }
           | {- empty -}                        { Nothing }

impspec: '(' exportlist ')'                     { (False, $2) }
       | 'hiding' '(' exportlist ')'            { (True, $3) }

-- Fixity Declarations --------------------------------------------------------
prec: {- empty -}                               { 9 :: Int }
    | TLITINT                           { case $1 of (i, _) -> fromIntegral i }

fixity: 'infixl'                                { Infixl }
      | 'infixr'                                { Infixr }
      | 'infix'                                 { Infix  }

ops: ops ',' op                                 { $1 ++ [$3] }
   | op                                         { [$1] }

-- Top-Level Declarations -----------------------------------------------------
topdecls: topdecls ';' topdecl                  { $1 ++ [$3] }
        | topdecls ';'                          { $1 }
        | topdecl                               { [$1] }

topdecl: cl_decl                                { $1 }
       | ty_decl                                { $1 }
       | 'instance' inst_type where_inst        { InstDecl Nothing $2 $3 }
       | 'instance' context '=>' inst_type where_inst        
                                                { InstDecl (Just $2) $4 $5}
       | 'default' '(' comma_types0 ')'         { DefaultDecl $3 }
       | 'foreign' fdecl                        { ForeignDecl $2 }
       | decl                                   { $1 }

cl_decl: 'class' tycl_hdr where_cls             { ClassDecl $2 $3 }

ty_decl: 'type' type '=' type                   { SynonymDecl $2 $4 }
       | data_or_newtype tycl_hdr constrs deriving  { $1 $2 $3 $4 }

data_or_newtype: 'data'                         { DataDecl }
               | 'newtype'                      { NewtypeDecl }

tycl_hdr: context '=>' type                     { ((Just $1), $3) }
        | type                                  { (Nothing, $1) }

-- Class body
decl_cls: decl                                  { $1 }

decls_cls : decls_cls ';' decl_cls              { $3:$1 }
          | decls_cls ';'                       { $1 }
          | decl_cls                            { [$1] }
          | {- empty -}                         { [] }

decllist_cls
  : '{'     decls_cls '}'                       { reverse $2 }
  | vocurly decls_cls close                     { reverse $2 }

where_cls: 'where' decllist_cls                 { $2 }
         | {- empty -}                          { [] }

-- Instance body
decl_inst: decl                                 { $1 }

decls_inst: decls_inst ';' decl_inst            { $3:$1 }
          | decls_inst ';'                      { $1 }
          | decl_inst                           { [$1] }
          | {- empty -}                         { [] }

decllist_inst
  : '{'     decls_inst '}'                      { reverse $2 }
  | vocurly decls_inst close                    { reverse $2 }

where_inst: 'where' decllist_inst               { $2 }
          | {- empty -}                         { [] }

-- Declarations
decls: decls ';' decl                           { $1 ++ [$3] }
     | decls ';'                                { $1 }
     | decl                                     { [$1] }
     | {- empty -}                              { [] }

decllist
  : '{'     decls '}'                           { $2 }
  | vocurly decls close                         { $2 }

-- Binding groups
binds: decllist                                 { $1 }

wherebinds: 'where' binds                       { $2 }
          | {- empty -}                         { [] }

-- Foreign import(/export) declarations ---------------------------------------
fdecl: 'import' callconv safety fspec           { FImDecl $2 (Just $3) $4 }
     | 'import' callconv        fspec           { FImDecl $2 Nothing   $3 }

callconv: varid                                 { $1 }

safety: 'unsafe'                                { Unsafe }
      | 'safe'                                  { Safe }

fspec: TLITSTR var '::' sigtypedoc         { FSpec (Just $ mkString $1) $2 $4 }
     |         var '::' sigtypedoc              { FSpec Nothing $1 $3 }

-- Type signatures ------------------------------------------------------------
sigtypedoc: context '=>' type                   { ((Just $1), $3) }
          | type                                { (Nothing, $1) }

sig_vars: sig_vars ',' var                      { $1 ++ [$3] }
        | var                                   { [$1] }

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
     | '{' fielddecls '}'                       { RecTy $2 }

inst_type: type                                 { $1 }

inst_types1: inst_type                          { [$1] }
           | inst_type ',' inst_types1          { $1:$3 }

comma_types0: comma_types1                      { $1 }
            | {- empty -}                       { [] }

comma_types1: type                              { [$1] }
            | type ',' comma_types1             { $1:$3 }

-- Datatype declarations ------------------------------------------------------
constrs: '=' constrs1                           { $2 }

constrs1: constrs1 '|' constr                   { $3:$1 }
        | constr                                { [$1] }

constr: constr_stuff                            { $1 }

constr_stuff
 : btype                                        { Con $1 }
 | btype conop btype                            { InfixCon $1 $2 $3 }

fielddecls: {- empty -}                         { [] }
          | fielddecls1                         { $1 }

fielddecls1
 : fielddecl ',' fielddecls1                    { $1 ++ $3 }
 | fielddecl                                    { $1 }

fielddecl: sig_vars '::' type             { [ConDeclField fld $3 | fld <- $1] }

deriving: 'deriving' qtycon                     { Just [Tycon $2] }
        | 'deriving' '(' ')'                    { Just [] }
        | 'deriving' '(' inst_types1 ')'        { Just $3 }
        | {- empty -}                           { Nothing }

-- Value definitions ----------------------------------------------------------
decl: sigdecl                                   { $1 }
    | infixexp rhs                              { ValDecl $1 $2 }

rhs: '=' exp wherebinds                         { UnguardedRhs $2 $3 }
   | gdrhs   wherebinds                         { GuardedRhs $1 $2 }

gdrhs: gdrhs gdrh                               { $1 ++ [$2] }
     | gdrh                                     { [$1] }

gdrh: '|' guardquals '=' exp                    { ($2, $4) }

sigdecl
  : sig_vars '::' sigtypedoc                    { TypeSigDecl $1 $3 }
  | fixity prec ops                             { FixSigDecl $1 $2 $3 }

-- Expressions ----------------------------------------------------------------
exp: infixexp '::' sigtypedoc                   { ExpWithTySig $1 $3 }
   | infixexp                                   { $1 }

infixexp: exp10                                 { $1 }
        | infixexp qop exp10                    { InfixExp $1 $2 $3 }

exp10
  : '\\' apat apats '->' exp                    { LamExp ($2:$3) $5 }
  | 'let' binds 'in' exp                        { LetExp $2 $4 }
  | 'if' exp semi_opt 'then' exp semi_opt 'else' exp
                                                { IfExp $2 $5 $8 }
  | 'case' exp 'of' altslist                    { CaseExp $2 $4 }
  | '-' fexp                                    { UMinusExp $2 }
  | 'do' stmtlist                               { DoExp $2 }
  | fexp                                        { $1 }

semi_opt: ';'                                   { () }
        | {- empty -}                           { () }

fexp: fexp aexp                                 { FunAppExp $1 $2 }
    | aexp                                      { $1 }

aexp: qvar '@' aexp                             { AsPat $1 $3 }
    | '~' aexp                                  { LazyPat $2 }
    | aexp1                                     { $1 }

aexp1: aexp1 '{' fbinds '}'                     { RecConUpdate $1 $3 }
     | aexp2                                    { $1 }

aexp2
  : qcname                                      { VarExp $1 }
  | literal                                     { LitExp $1 }
  | '(' texp ')'                                { ParExp $2 }
  | '(' tup_exprs ')'                           { TupleExp $2 }
  | '[' list ']'                                { $2 }
  | '_'                                         { WildcardPat }

-- Tuple expressions ----------------------------------------------------------
texp: exp                                       { $1 }
    | infixexp qop                              { SectionL $1 $2 }
    | qopm infixexp                             { SectionR $1 $2 }

tup_exprs: texp commas_tup_tail                 { Just $1 : $2 }
         | commas tup_tail                      { replicate ($1-1) Nothing ++ $2 }

commas_tup_tail: commas tup_tail                { replicate ($1-1) Nothing ++ $2 }

tup_tail: texp commas_tup_tail                  { Just $1 : $2 }
        | texp                                  { [Just $1] }
        | {- empty -}                           { [Nothing] }

-- List expressions -----------------------------------------------------------
list: texp                                      { ListExp [$1] }
    | lexps                                     { ListExp (reverse $1) }
    | texp '..'                                 { ArithSeqExp (From $1) }
    | texp ',' exp '..'                        { ArithSeqExp (FromThen $1 $3) }
    | texp '..' exp                             { ArithSeqExp (FromTo $1 $3) }
    | texp ',' exp '..' exp               { ArithSeqExp (FromThenTo $1 $3 $5) }
    | texp '|' squals                           { ListCompExp $1 $3 }

lexps: lexps ',' texp                           { $3:$1 }
     | texp ',' texp                            { [$3, $1] }

-- Guards / List Comprehensions -----------------------------------------------
squals: squals ',' qual                         { $1 ++ [$3] }
      | qual                                    { [$1] }

guardquals: squals                              { $1 }

-- Case alternatives ----------------------------------------------------------
altslist
  : '{'     alts '}'                            { reverse $2 }
  | vocurly alts close                          { reverse $2 }

alts: alts1                                     { $1 }
    | ';' alts                                  { $2 }

alts1: alts1 ';' alt                            { $3:$1 }
     | alts1 ';'                                { $1 }
     | alt                                      { [$1] }

alt: pat alt_rhs                                { Match $1 $2 }

alt_rhs: '->' exp wherebinds                    { UnguardedRhs $2 $3 }
       | gdpats   wherebinds                    { GuardedRhs $1 $2 }

gdpats: gdpats gdpat                            { $1 ++ [$2] }
      | gdpat                                   { [$1] }

gdpat: '|' guardquals '->' exp                  { ($2, $4) }

pat: exp                                        { $1 }

apat: aexp                                      { $1 }

apats: apat apats                               { $1:$2 }
     | {- empty -}                              { [] }

-- Statement sequences --------------------------------------------------------
stmtlist
  : '{'     stmts '}'                           { $2 }
  | vocurly stmts close                         { $2 }

stmts: stmt stmts_help                          { $1:$2 }
     | ';' stmts                                { $2 }
     | {- empty -}                              { [] }

stmts_help: ';' stmts                           { $2 }
          | {- empty -}                         { [] }

stmt: qual                                      { $1 }

qual: pat '<-' exp                              { BindStmt $1 $3 }
    | exp                                       { ExpStmt $1 }
    | 'let' binds                               { LetStmt $2 }

-- Record Field Updata/Construction -------------------------------------------
fbinds: fbinds1                                 { $1 }
      | {- empty -}                             { ([], False) }

fbinds1: fbind ',' fbinds1
                                          { case $3 of (fs, b) -> ($1 : fs,b) }
       | fbind                                  { ([$1], False) }
       | '..'                                   { ([], True) }

fbind: qvar '=' exp                             { RecField $1 $3 }

-- Data constructors ----------------------------------------------------------
qcon: qconid                                    { $1 }
    | '(' qconsym ')'                           { $2 }
    | sysdcon                                   { $1 }

sysdcon
  : '(' ')'                                     { mkCName ("()", $1) }
  | '(' commas ')'             { mkCName ("(" ++ replicate $2 ',' ++ ")", $1) }
  | '[' ']'                                     { mkCName ("[]", $1) }

conop: consym                                   { $1 }
     | '`' conid '`'                            { $2 }

qconop: qconsym                                 { $1 }
      | '`' qconid '`'                          { $2 }

-- Type constructors ----------------------------------------------------------
gtycon -- A "general" qualified tycon
  : oqtycon                                     { $1 }
  | '(' ')'                                     { mkCName ("()", $1) }
  | '(' commas ')'             { mkCName ("(" ++ replicate $2 ',' ++ ")", $1) }
  | '(' '->' ')'                                { mkCName ("(->)", $1) }
  | '[' ']'                                     { mkCName ("[]", $1) }

oqtycon -- An "ordinary" qualified tycon
  : qtycon                                      { $1 }
  | '(' qtyconsym ')'                           { $2 }
  | '(' '~' ')'                                 { mkCName ("~", $2) }

qtycon: TQCONID                                 { mkCName $1 }
      | tycon                                   { $1 }

tycon: TCONID                                   { mkCName $1 }

qtyconsym: TQCONSYM                             { mkCName $1 }
         | tyconsym                             { $1 }

tyconsym: TCONSYM                               { mkCName $1 }

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
  : TVARID                                      { mkVName $1 }
  | 'as'                                        { mkVName ("as", $1) }
  | 'hiding'                                    { mkVName ("hiding", $1) }
  | 'qualified'                                 { mkVName ("qualified", $1) }
  | 'safe'                                      { mkVName ("safe", $1) }
  | 'unsafe'                                    { mkVName ("unsafe", $1) }

tyvarsym: TVARSYM                               { mkVName $1 }

-- Variables ------------------------------------------------------------------
var: varid                                      { $1 }
   | '(' varsym ')'                             { $2 }

qvar: qvarid                                    { $1 }
    | '(' varsym ')'                            { $2 }
    | '(' qvarsym1 ')'                          { $2 }

qvarid: varid                                   { $1 }
      | TQVARID                                 { mkVName $1 }

varid
  : TVARID                                      { mkVName $1 }
  | 'as'                                        { mkVName ("as", $1) }
  | 'hiding'                                    { mkVName ("hiding", $1) }
  | 'qualified'                                 { mkVName ("qualified", $1) }
  | 'safe'                                      { mkVName ("safe", $1) }
  | 'unsafe'                                    { mkVName ("unsafe", $1) }

qvarsym: varsym                                 { $1 }
       | qvarsym1                               { $1 }

qvarsym_no_minus
  : varsym_no_minus                             { $1 }
  | qvarsym1                                    { $1 }

qvarsym1: TQVARSYM                              { mkVName $1 }

varsym: varsym_no_minus                         { $1 }
      | '-'                                     { mkVName ("-", $1) }

varsym_no_minus: TVARSYM                        { mkVName $1 }
               | '!'                            { mkVName ("!", $1) }

-- Data constructors ----------------------------------------------------------
qconid: conid                                   { $1 }
      | TQCONID                                 { mkCName $1 }

conid: TCONID                                   { mkCName $1 }

qconsym: consym                                 { $1 }
       | TQCONSYM                               { mkCName $1 }

consym: TCONSYM                                 { mkCName $1 }
      | ':'                                     { mkCName (":", $1) }

-- Literals -------------------------------------------------------------------
literal: TLITCHAR                               { mkChar $1 }
  |      TLITSTR                                { mkString $1 }
  |      TLITINT                                { mkInteger $1 }
  |      TLITFLOAT                              { mkFloat $1 }

-- Layout ---------------------------------------------------------------------
close: vccurly                                  { () }
     | error                                    {% popCtx }

-- Misc -----------------------------------------------------------------------
commas: commas ','                              { $1 + 1 }
      | ','                                     { 1 }

modid: TCONID                                   { mkCName $1 }
     | TQCONID                                  { mkCName $1 }
{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser
}
