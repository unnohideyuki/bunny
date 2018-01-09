module PpType where

import           Types

ppType :: Type -> String

ppType (TAp (TAp (TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))) t1) t2) =
  "(" ++ ppType t1 ++ " -> " ++ ppType t2 ++ ")"

ppType (TAp (TCon (Tycon "[]" (Kfun Star Star))) t) = "[" ++ ppType t ++ "]"

ppType (TVar (Tyvar n Star)) = n

ppType t = show t
