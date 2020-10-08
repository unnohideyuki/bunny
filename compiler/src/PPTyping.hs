module PPTyping where

import           PPTypes
import           Typing

import           Control.Monad
import           Text.PrettyPrint.ANSI.Leijen

ddumpRen :: [BindGroup] -> IO ()
ddumpRen bgs = do
  putStrLn "\n---- ddumpRen ----"
  forM_ (zip [0..] bgs) $ \(i, bg) -> do
    putStrLn $ "---- BindGroup#" ++ show i
    putStrLn $ show $ ppBg bg

ppBg :: BindGroup -> Doc
ppBg (es, iss) = do
  nest 0 (text "-- [Expl]" <$$>
          foldr (<$$>) empty (map ppExpl es) <$$>
          text "-- [[Impl]]" <$$>
          foldr
           (<$$>)
           empty
           (map (\(i, is) -> text ("-- Is#" ++ show i) <$$>
                             foldr (<$$>) empty (map ppImpl is)) (zip [0..] iss)))

ppExpl :: Expl -> Doc
ppExpl (n, sc, alts) =
  text n <+> text "::" <+> ppScheme sc <$$> empty <$$> ppImpl (n, alts)

ppImpl :: Impl -> Doc
ppImpl (n, alts) =
  foldr (<$$>) empty (map (\alt -> text n <+> ppAlt alt) alts)

ppAlt :: Alt -> Doc
ppAlt (pats, expr) =
  list (map ppPat pats) <+> nest 4 (text "=" <$$> ppExpr expr)

ppAssump :: Assump -> Doc
ppAssump (n :>: sc) = text n <+> text ":>:" <+> ppScheme sc

ppLiteral :: Literal -> Doc
ppLiteral (LitInt i)  = text "LitInt"  <+> text (show i)
ppLiteral (LitChar c) = text "LitChar" <+> text (show c)
ppLiteral (LitFrac d) = text "LitFrac" <+> text (show d)
ppLiteral (LitStr s)  = text "LitStr"  <+> text (show s)

ppExpr :: Expr -> Doc
ppExpr (Var n) = text "Var" <+> text n

ppExpr (Lit l) = text "Lit (" <> ppLiteral l <> text ")"

ppExpr (Const a) = text "Const" <+> ppAssump a

ppExpr (Ap f e) =
  text "AP" <+> text "(" <> ppExpr f <> text ")" <+> text "(" <> ppExpr e <> text ")"

ppExpr (Let bg e) =
  text "Let" <+> nest 4 (ppBg bg) <$$> nest 2 (text "in" <$$> ppExpr e)

ppPat (PVar n) = text "PVar" <+> text n

ppPat (PWildcard) = text "PWildcard"

ppPat (PAs n pat) = text n <> text "@(" <> ppPat pat <> text ")"

ppPat (PLit l) = text "PLit" <+> ppLiteral l

ppPat (PCon a pats) =
  text "PCon" <+> text "(" <> ppAssump a <> text ")" <+>
  text "[" <> list (map ppPat pats) <> text "]"
