module CodeGen where

import STG

emitPreamble =
  let
     preamble = [ "import jp.ne.sakura.uhideyuki.brt.brtsyn.*;"
                , "import jp.ne.sakura.uhideyuki.brt.runtime.*;"
                , ""
                , "public class Sample {"
                , "    public static void main(String[] args){"
                , "      RT.eval(Main.mkmain());"
                , "    }"
                , "}"
                , ""
                ]
     ploop [] = return ()
     ploop (s:ss) = do {putStrLn s; ploop ss}
  in
   ploop preamble
   
emitProgram :: Program -> IO ()
emitProgram prog = do
  emitPreamble
  emitBinds prog

emitBinds [] = return ()
emitBinds (b:bs) = do
  emitHeader b
  emitBind b
  emitBinds bs
  emitFooter

emitHeader (Bind (TermVar s) _) = putStrLn $ "class " ++ m ++ " {"
  where m = takeWhile (/= '.') s -- todo: deeper module name

emitFooter = putStrLn "}"

emitBind (Bind (TermVar n) e) = do
  putStrLn $ "    public static Expr mk" ++ n' ++ "(){"
  putStrLn $ genBody e
  putStrLn $ "    }"
  putStrLn $ ""
  where
    m = takeWhile (/= '.') n -- todo: deeper module name
    n' = drop (length m + 1) n

genBody e = "      return " ++ genExpr e ++ ";"

genExpr e@(AtomExpr _) = genAtomExpr e

genExpr (FunAppExpr f [e]) = 
  "RTLib.app(\n" ++ genExpr f ++ ",\n" ++ genExpr e ++ "\n)"


genExpr e = error $ "Non-exaustive pattern in genExpr: " ++ show e

genAtomExpr (AtomExpr (VarAtom (TermVar n)))
  | n == "Prim.putStrLn" = "RTLib.putStrLn"
  | n == "Prim.:"        = "RTLib.cons"
  | n == "Prim.[]"       = "RTLib.nil"
  | otherwise            = error $ "Function not found at genAtomExpr: " ++ n

genAtomExpr (AtomExpr (LitAtom (LitStr s))) =
  "RTLib.fromJString(" ++ show s ++ ")"

genAtomExpr (AtomExpr (LitAtom (LitChar c))) =
  "RTLib.fromChar(" ++ show c ++ ")"

genAtomExpr e = error $ "Non-exhaustive pattern in genAtomExpr: " ++ show e
  
