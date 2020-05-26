module Types where

import qualified Data.Map.Strict as Map
import           Symbol

data Kind = Star | Kfun Kind Kind deriving (Eq, Show)

data Type = TVar Tyvar | TCon Tycon | TAp Type Type | TGen Int
          deriving (Eq, Show)

data Tyvar = Tyvar Id Kind deriving (Eq, Show)
data Tycon = Tycon Id Kind deriving (Eq, Show)

tUnit    :: Type
tUnit     = TCon (Tycon "()" Star)
tChar    :: Type
tChar     = TCon (Tycon "Prelude.Char" Star)
tInt     :: Type
tInt      = TCon (Tycon "Prelude.Int" Star)
tInteger :: Type
tInteger  = TCon (Tycon "Prelude.Integer" Star)
tFloat   :: Type
tFloat    = TCon (Tycon "Prelude.Float" Star)
tDouble  :: Type
tDouble   = TCon (Tycon "Prelude.Double" Star)

tBool    :: Type
tBool     = TCon (Tycon "Prelude.Bool" Star)

tList    :: Type
tList     = TCon (Tycon "Prelude.[]" (Kfun Star Star))
tArrow   :: Type
tArrow    = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))
tTuple2  :: Type
tTuple2   = TCon (Tycon "Prelude.(,)" (Kfun Star (Kfun Star Star)))

tString :: Type
tString  = list tChar

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

list :: Type -> Type
list = TAp tList

pair :: Type -> Type -> Type
pair a = TAp (TAp tTuple2 a)
