module CodeGen where

import STG

emitPreamble =
  let
     preamble = [ "import jp.ne.sakura.uhideyuki.brt.brtsyn.*;"
                , "import jp.ne.sakura.uhideyuki.brt.runtime.*;"
                , ""
                , "public class Sample {"
                , "    public static void main(String[] args){"
                , "      RT.eval(Main.main);"
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
  printBody ss
  putStrLn $ "    }"
  putStrLn $ ""
  putStrLn $ "    public static Expr " ++ n' ++ " = mk" ++ n' ++ "();"
  where
    m = takeWhile (/= '.') n -- todo: deeper module name
    n' = drop (length m + 1) n
    ss = genBody e
    printBody [] = return ()
    printBody (s:ss) = do{putStrLn $ "      " ++ s; printBody ss}

genBody (FunAppExpr f [e]) =
  [ "Expr e1 = " ++ genExpr f ++ ";"
  , "Expr e2 = " ++ genAtomExpr e ++ ";"
  , "return RTLib.app(e1, e2);"
  ]

genExpr e@(AtomExpr _) = genAtomExpr e

genAtomExpr (AtomExpr (VarAtom (TermVar n)))
  | n == "Prim.putStrLn" = "RTLib.putStrLn"

genAtomExpr (AtomExpr (LitAtom (LitStr s))) =
  "RTLib.fromJString(" ++ show s ++ ")"
  
