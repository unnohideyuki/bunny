module Absyn where
import           Lexer
import           Symbol

data Module = Module { modid   :: Maybe Name
                     , exports :: Maybe [IE]
                     , body    :: ([ImportDecl], [Decl])
                     }
             deriving Show

data IE
  = IEVar            Name
  | IEThingAbs       Name        -- ^ Class/Type
  | IEThingAll       Name        -- ^ Class/Type plus all methods/constructors
  | IEThingWith      Name [Name] -- ^ Class/Type plus some methods/constructors
  | IEModuleContents Name        -- ^ Module (export only)
  deriving Show

data ImportDecl = ImportDecl Bool Name (Maybe Name) (Maybe (Bool, [IE]))
                deriving Show

data Decl = ValDecl     Exp Rhs
          | TypeSigDecl [Name] (Maybe Type, Type)
          | FixSigDecl  Fixity Int [Name]
          | DefaultDecl [Type]
          | ForeignDecl FDecl
          | ClassDecl   (Maybe Type, Type) [Decl]
          | SynonymDecl Type Type
          | DataDecl    (Maybe Type, Type) [Constr] (Maybe [Type])
          | NewtypeDecl (Maybe Type, Type) [Constr] (Maybe [Type])
          | InstDecl    (Maybe Type) Type [Decl]
          deriving Show

data FDecl = FImDecl Name (Maybe Safety) FSpec
           deriving Show

data Safety = Safe | Unsafe
            deriving (Eq, Show)

data FSpec = FSpec (Maybe Literal) Name (Maybe Type, Type)
           deriving Show

data Stmt = BindStmt Exp Exp
          | ExpStmt  Exp
          | LetStmt  [Decl]
          deriving Show

data Rhs = UnguardedRhs Exp [Decl]
         | GuardedRhs   [([Stmt],Exp)] [Decl]
         deriving Show

data Alt = Match Exp Rhs
         deriving Show

data Fixity = Infixl | Infixr | Infix
            deriving (Show, Eq)

data Literal = LitInteger Integer Pos
             | LitFloat   Float   Pos
             | LitString  String  Pos
             | LitChar    Char    Pos
               deriving Show

-- todo Exp is just a place holder
data RecField = RecField Name Exp
                deriving Show

data ConDeclField = ConDeclField Name Type
                  deriving Show

data Type = Tyvar   Name
          | Tycon   Name
          | FunTy   Type Type
          | AppTy   Type Type
          | BangTy  Type
          | TupleTy [Type]
          | ListTy  Type
          | ParTy   Type -- why needed parened type?
          | RecTy   [ConDeclField]
          deriving Show

data Constr = Con      Type
            | InfixCon Type Name Type
            deriving Show
{- メモ
   いまは、パターンも何もかも Exp にしているが、
   Pattern と Exp は別の型にするべきかもしれない。
   そのときには、mkLamExp とかのなかで Exp -> Pat の変換をやるのだろう。
-}
data Exp = Dummy -- dummy
         | ParExp       Exp
         | TupleExp     [Maybe Exp]

         | VarExp       Name
         | LitExp       Literal
         | FunAppExp    Exp Exp
         | InfixExp     Exp Name Exp
         | LamExp       [Exp] Exp
         | LetExp       [Decl] Exp
         | IfExp        Exp Exp Exp
         | CaseExp      Exp [Alt]
         | UMinusExp    Exp
         | DoExp        [Stmt]
         | ExpWithTySig Exp (Maybe Type, Type)

         | AsPat        Name Exp
         | LazyPat      Exp
         | WildcardPat
         | RecConUpdate Exp ([RecField], Bool)

         | SectionL     Exp Name
         | SectionR     Name Exp

         | ListExp      [Exp]
         | ArithSeqExp  ArithSeqRange
         | ListCompExp  Exp [Stmt]
           deriving Show

data ArithSeqRange = From       Exp
                   | FromThen   Exp Exp
                   | FromTo     Exp Exp
                   | FromThenTo Exp Exp Exp
                   deriving Show

---- Helper functions
mkVName :: (String, AlexPosn) -> Name
mkVName (s, pos) = Name { origName = s
                        , namePos  = extrPos pos
                        , isConName = False
                        }

mkCName :: (String, AlexPosn) -> Name
mkCName (s, pos) = Name { origName = s
                        , namePos  = extrPos pos
                        , isConName = True
                        }

extrPos :: AlexPosn -> Pos
extrPos (AlexPn _ line col) = (line, col)

mkChar :: (Char, AlexPosn) -> Literal
mkChar (c, pos) = LitChar c $ extrPos pos

mkString :: (String, AlexPosn) -> Literal
mkString (s, pos) = LitString s $ extrPos pos

mkInteger :: (Integer, AlexPosn) -> Literal
mkInteger (i, pos) = LitInteger i $ extrPos pos

mkFloat :: (Float, AlexPosn) -> Literal
mkFloat (d, pos) = LitFloat d $ extrPos pos
