module PPTyping where

import           PPTypes
import           Types
import           Typing

import           Control.Monad
import           Text.PrettyPrint.ANSI.Leijen

ddumpRen :: [BindGroup] -> IO ()
ddumpRen bgs = do
  putStrLn "\n---- ddumpRen ----"
  forM_ (zip [0..] bgs) $ \(i, bg) -> do
    putStrLn $ "---- BindGroup#" ++ show i
    ddumpBg bg

ddumpBg :: BindGroup -> IO ()
ddumpBg (es, iss) = do
  putStrLn "---- [Expl]"
  forM_ es $ \expl -> putStrLn $ show $ ppExpl expl
  putStrLn "\n---- [[Impl]]"
  forM_ (zip [0..] iss) $ \(i, is) -> do
    putStrLn $ "---- Is#" ++ show i
    forM_ is $ \impl -> putStrLn $ show $ ppImpl impl

ppExpl :: Expl -> Doc
ppExpl (n, sc, alts) =
  text n <+> text "::" <+> ppScheme sc <$$> empty <$$> ppImpl (n, alts)

ppImpl :: Impl -> Doc
ppImpl (n, alts) =
  foldr (<$$>) empty (map (\alt -> text n <+> text "=" <+> ppAlt alt) alts)


ppAlt :: Alt -> Doc
ppAlt (pats, expr) = text "ppAlt"




