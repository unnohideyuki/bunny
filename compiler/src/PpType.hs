module PpType where

import           Types

ppType :: Type -> String

ppType (TAp (TAp (TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))) t1) t2) =
  "(" ++ ppType t1 ++ " -> " ++ ppType t2 ++ ")"

ppType (TAp (TCon (Tycon "[]" (Kfun Star Star))) t) = "[" ++ ppType t ++ "]"

ppType (TAp t1 t2) = ppType t1 ++ " " ++ ppType t2

ppType (TVar (Tyvar n Star)) = n

ppType (TCon (Tycon n _)) = n

ppType (TGen n) = "g" ++ show n

ppType t = show t
