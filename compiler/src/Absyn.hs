module Absyn where

type Pos = (Int, Int) -- line number, column number

posLine :: Pos -> Int
posLine (line, _) = line
posCol :: Pos -> Int
posCol (_, col) = col

data Name = Name { name_body :: String
                 , name_qual :: String
                 , name_pos  :: Pos
                 }
          deriving Show

data Module = Module { modid    :: Maybe Name
                     , exports  :: Maybe [IE]
                     , body     :: ([ImportDecl], [Decl])
                     }
             deriving Show

-- | Imported or exported entity
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
          | InstDecl    Type [Decl]
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

data Fixity = Infixl | Infixr | Infix
            deriving Show

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
         | IfExp        Exp Exp Exp
         | UMinusExp    Exp
         | ExpWithTySig Exp -- todo: Sig

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
