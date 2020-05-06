module PPCore where

import           Core
import           PPTypes

import           Text.PrettyPrint.ANSI.Leijen

-- Var

ppVar :: Var -> Doc

ppVar (TermVar n qt) =
  text "(" <> text n <+> text "::" <+> ppQType qt <> text ")"

ppVar (DictVar x y) =
  text "${" <> text x <+> text y <> text "}"

ppVar (CompositDict d ds) =
  text "(CompositDict" <+> ppExpr d <+> text "[" <> ppExprs "," ds <> text "])"


ppVars :: String -> [Var] -> Doc
ppVars _ []     = empty
ppVars _ [v]    = ppVar v
ppVars s (v:vs) = ppVar v <> text s <> ppVars s vs

-- Literal

ppLit :: Literal -> Doc

ppLit (LitInt n t) =
  text "(" <> text (show n) <+> text "::" <+> ppType t <> text ")"

ppLit (LitChar c _) = text $ show c

ppLit (LitStr s _) = text $ show s

ppLit l = text $ show l

-- Expr

ppExpr :: Expr -> Doc

ppExpr (Var v) = ppVar v

ppExpr (Lit l) = ppLit l

-- TODO: improve!
ppExpr (App (App (Var (TermVar "Prelude.$" _)) e1) e2) =
  nest 2
  (text "(" <> ppExpr e1 <+> text "$" <+> softline <> ppExpr e2 <> text ")")

ppExpr (App (App (Var (TermVar "Prelude.." _)) e1) e2) =
  (text "(" <> ppExpr e1 <+> text "." <+> ppExpr e2 <> text ")")

{-
ppExpr (App (App (App (Var (TermVar "Prelude.>>" _)) d) e1) e2) =
  nest 0
  (text "(" <> ppExpr e1 <+> text ">>" <+> ppExpr d <$$>
   ppExpr e2 <> text ")")
-}

ppExpr (App e1 e2) =
  nest 2 (text "(" <> ppExpr e1 <+> softline <> ppExpr e2 <> text ")")

ppExpr (Lam vs' e) =
  nest 4
  (text "\\" <> ppVars " " vs' <+> text "->" <$$> ppExpr e)

ppExpr (Let b e) =
  nest 0
  (nest 4 (text "let" <$$> ppBind b) <$$> (nest 2 (text "in" <$$> ppExpr e)))

ppExpr (Case e v as) =
  group
  (nest 4
   (text "case" <+> ppExpr e <+> ppVar v <+> text "of" <$$>
    nest 0 (ppAlts as)))


ppExprs :: String -> [Expr] -> Doc
ppExprs _ []     = empty
ppExprs _ [v]    = ppExpr v
ppExprs s (v:vs) = ppExpr v <> text s <> ppExprs s vs


ppAlt :: Alt -> Doc
ppAlt (ac, vs, e) =
  nest 4
  (ppAltCon ac <+> ppVars " " vs <> text "->" <+> softline <> ppExpr e)

ppAlts :: [Alt] -> Doc
ppAlts []     = empty
ppAlts (a:as) = ppAlt a <$$> ppAlts as

ppAltCon :: AltCon -> Doc

ppAltCon (DataAlt (DataCon n vs qt)) =
  group
  (text n <+> ppVars " " vs <+> text "::" <+> ppQType qt)

ppAltCon a = text (show a)

ppBind :: Bind -> Doc
ppBind (Rec bs) = ppbs bs
  where
    ppbs ((v, e) : bs') =
      nest 0 (nest 4 ((ppVar v <+> text "=") <$$> ppExpr e) <$$> empty <$$>
              ppbs bs')
    ppbs [] = empty


