module Types where

import Symbol

data Kind = Star | Kfun Kind Kind
          deriving (Eq, Show)

data Type = TVar Tyvar | TCon Tycon | TAp Type Type | TGen Int
          deriving (Eq, Show)

data Tyvar = Tyvar Id Kind
           deriving (Eq, Show)

data Tycon = Tycon Id Kind
           deriving (Eq, Show)

tUnit    :: Type
tUnit     = TCon (Tycon "()" Star)
tChar    :: Type
tChar     = TCon (Tycon "Prelude.Char" Star)
tInt     :: Type
tInt      = TCon (Tycon "Int" Star)
tInteger :: Type
tInteger  = TCon (Tycon "Prelude.Integer" Star)
tFloat   :: Type
tFloat    = TCon (Tycon "Float" Star)
tDouble  :: Type
tDouble   = TCon (Tycon "Double" Star)

tBool    :: Type
tBool     = TCon (Tycon "Bool" Star)

tList    :: Type
tList     = TCon (Tycon "[]" (Kfun Star Star))
tArrow   :: Type
tArrow    = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))
tTuple2  :: Type
tTuple2   = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))

tString :: Type
tString  = list tChar

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

list :: Type -> Type
list t = TAp tList t

pair :: Type -> Type -> Type
pair a b = TAp (TAp tTuple2 a) b
