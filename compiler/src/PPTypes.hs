module PPTypes where

import           Types
import           Typing

import           Text.PrettyPrint.ANSI.Leijen

-- Type

ppType :: Type -> Doc

ppType (TAp (TAp (TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))) t1) t2) =
  text "(" <> ppType t1 <+> text "->" <+> ppType t2 <> text ")"

ppType (TAp (TCon (Tycon "[]" (Kfun Star Star))) t) =
  text "[" <> ppType t <> text "]"

ppType (TAp t1 t2) =
  text "(" <> ppType t1 <+> ppType t2 <> text ")"

ppType (TVar (Tyvar n Star)) = text n

ppType (TCon (Tycon n _)) = text n

ppType (TGen n) = char $ ['a', 'b'..] !! n

ppType t = text $ show t

showType :: Type -> String
showType = show . ppType

-- Pred

ppPred :: Pred -> Doc
ppPred (IsIn n t) = text n <+> ppType t

ppPreds :: [Pred] -> Doc
ppPreds ps = text "[" <> pppreds' ps <> text "]"
  where pppreds' [] = empty
        pppreds' [x] = ppPred x
        pppreds' (x:xs) = ppPred x <> text "," <> pppreds' xs

-- Qual Type

ppQType :: Qual Type -> Doc

ppQType ([] :=> t) = ppType t

ppQType (ps :=> t) =
  text "(" <> ppPreds ps <+> text ":=>" <+> ppType t <> text ")"

-- Scheme

ppScheme :: Scheme -> Doc

ppScheme (Forall _ ([] :=> t)) = ppType t

ppScheme (Forall ks (ps :=> t)) =
  text "Forall" <+> text (show ks) <+>
  text "(" <> ppPreds ps <+> text ":=>" <+> ppType t <> text ")"

showScheme :: Scheme -> String
showScheme = show . ppScheme

