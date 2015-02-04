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

data Decl = Decl
            deriving Show

data Literal = LitInteger Integer Pos
             | LitFloat   Float   Pos
             | LitString  String  Pos
             | LitChar    Char    Pos

-- todo Exp is just a place holder
data RecField = RecField Name Exp

data Exp = Exp
