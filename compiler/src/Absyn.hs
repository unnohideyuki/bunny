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
                     , exports  :: [IE]
                     , impdecls :: [Importdecl]
                     , decls    :: [Decl]
                     }
             deriving Show

-- | Imported or exported entity
data IE
  = IEVar           Name
  | IEThingAbs      Name        -- ^ Class/Type
  | IEThingAll      Name        -- ^ Class/Type plus all methods/constructors
  | IEThingWith     Name [Name] -- ^ Class/Type plus some methods/constructors
  | IEModuleContens Name        -- ^ Module (export only)
  deriving Show

data Importdecl = Importdecl
                deriving Show

data Decl = ValDecl     Exp Rhs
          | TypeSigDecl [Name] Context
          | FixSigDecl  Fixity Int [Name]
          deriving Show

data Qual = DummyQual
          deriving Show

data Rhs = UnguardedRhs Exp [Decl]
         | GuardedRhs   [([Qual],Exp)] [Decl]
         deriving Show

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

data Type = Tyvar   Name
          | Tycon   Name
          | FunTy   Type Type
          | AppTy   Type Type
          | BangTy  Type
          | TupleTy [Type]
          | ListTy  Type
          | ParTy   Type -- why needed parened type?
          deriving Show

data Context = Context (Maybe Type) Type
             deriving Show

{- メモ
   いまは、パターンも何もかも Exp にしているが、
   Pattern と Exp は別の型にするべきかもしれない。
   そのときには、mkLamExp とかのなかで Exp -> Pat の変換をやるのだろう。
-}
data Exp = Dummy -- dummy

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
           deriving Show
