module CodeGen where

import STG
import Symbol

import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.List (intersperse)

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

emitBind b = putStrLn $ result st
  where (_, st) = runState (genBind b) initGenSt

genBind (Bind (TermVar n) e) = do
  enterBind n
  genBody e
  exitBind

data GenSt = GenSt { str :: String
                   , idx :: Int
                   , env :: Map.Map Id Id
                   , gid :: Int
                   , sstack :: [String]
                   , istack :: [Int]
                   , estack :: [Map.Map Id Id]
                   , result :: String
                   }

enterBind :: String -> GEN ()
enterBind name = do
  st <- get
  let s = str st
      n = idx st
      ss = sstack st
      is = istack st
      st' = st{ str = "    public static Expr mk" ++ name' ++ "(){\n"
              , idx = 0
              , sstack = s:ss
              , istack = n:is
              }
  put st'
  where
    m = takeWhile (/= '.') name -- todo: deeper module name
    name' = drop (length m + 1) name

exitBind :: GEN ()
exitBind = do
  st <- get
  let curs = str st
      (s:ss) = sstack st
      (i:is) = istack st
      r = result st
      st' = st{ str = s
              , idx = i
              , sstack = ss
              , istack = is
              , result = r ++ curs ++ "    }\n\n"
              }
  put st'


initGenSt :: GenSt
initGenSt = GenSt { str = ""
                  , idx = 0
                  , env = Map.empty
                  , gid = 0
                  , sstack = []
                  , istack = []
                  , estack = []
                  , result = ""
                  }

type GEN a = State GenSt a

nexti :: GEN Int
nexti = do
  st <- get
  let i = idx st
  put st{idx = i + 1}
  return i

nextgid :: GEN Int
nextgid = do
  st <- get
  let i = gid st
  put st{gid = i + 1}
  return i

appendCode :: String -> GEN ()
appendCode code = do
  st <- get
  let s = str st
      s' = "      " ++ code ++ "\n"
  put st{str = s ++ s'}

genBody :: Expr -> GEN String
genBody e = do
  n <- genExpr e
  appendCode $ "return t" ++ show n ++ ";"
  st <- get
  return $ str st

genExpr :: Expr -> GEN Int

genExpr e@(AtomExpr _) = genAtomExpr e

genExpr (FunAppExpr f [e]) = do
  n1 <- genExpr f
  n2 <- genExpr e
  n <- nexti
  appendCode $
    "Expr t" ++ show n ++
    " = " ++ "RTLib.app(t" ++ show n1 ++ ", t" ++ show n2 ++ ");"
  return n

genExpr (LetExpr bs e) = do
  n <- genBs bs []
  let vs = map (\(Bind (TermVar n) _) -> n) bs
      ns = map (\i -> "args[" ++ show i ++ "]") [0..]
      nenv = fromList $ zip vs ns
  lamname <- genLambda (length bs) nenv e
  n' <- nexti
  let s = "new LetExpr(t" ++ show n ++ ", new " ++ lamname ++ "())"
  appendCode $ "Expr t" ++ show n' ++ " = " ++ s ++ ";"
  return n'
  where
    genBs [] ns = genArgs $ reverse ns
    genBs ((Bind _ e):bs) ns = do
      n <- genExpr e
      genBs bs (n:ns)

    genArgs ns = do
      n <- nexti
      let s0 = "Expr[] t" ++ show n ++ " = {"
          s1 = concat $ intersperse "," $ map (\i -> "t" ++ show i) ns
          s2 = "};"
      appendCode $ s0 ++ s1 ++ s2
      return n

genExpr e = error $ "Non-exaustive pattern in genExpr: " ++ show e

genLambda arty nenv expr = do
  i <- nextgid
  let lamname = "LAM" ++ show i
  enterLambda arty lamname nenv
  n <- genExpr expr
  exitLambda n
  return lamname
  where
    enterLambda arty name nenv = do
      st <- get
      let s = str st
          s' = "    public static class "
               ++ name ++ " implements LambdaForm {\n"
          s'' = "     public int arity(){ return " ++ show arty ++ "; }\n"
          s''' = "     public Expr call(AtomExpr[] args){\n"
          ss = sstack st
          n = idx st
          is = istack st
          oenv = env st
          es = estack st
          st' = st{ str = s' ++ s'' ++ s'''
                  , idx = 0
                  , env = nenv
                  , sstack = s:ss
                  , istack = n:is
                  , estack = oenv:es
                  }
      put st'

    exitLambda n = do
      appendCode $ "return t" ++ show n ++ ";"
      st <- get
      let curs = str st
          (s:ss) = sstack st
          (i:is) = istack st
          (_:es) = estack st
          r = result st
          st' = st{ str = s
                  , idx = i
                  , sstack = ss
                  , istack = is
                  , estack = es
                  , result = r ++ curs ++ "     }\n    }\n\n"
                  }
      put st'
                          

genAtomExpr (AtomExpr (VarAtom (TermVar n)))
  | n == "Prim.putStrLn" = emit "RTLib.putStrLn"
  | n == "Prim.:"        = emit "RTLib.cons"
  | n == "Prim.[]"       = emit "RTLib.nil"
  | otherwise            = do
    st <- get
    let h = env st
        v = case Map.lookup n h of
          Just s -> s
          Nothing -> error $ "Function not found at genAtomExpr: " ++ n
    emit v
  where
    emit s = do
      n <- nexti
      appendCode $ "Expr t" ++ show n ++ " = " ++ s ++ ";"
      return n

genAtomExpr (AtomExpr (LitAtom (LitStr s))) = do
  n <- nexti
  appendCode $ "Expr t" ++ show n ++ " = RTLib.fromJString(" ++ show s ++ ");"
  return n

genAtomExpr (AtomExpr (LitAtom (LitChar c))) = do
  n <- nexti
  appendCode $ "Expr t" ++ show n ++ " = RTLib.fromChar(" ++ show c ++ ");"
  return n

genAtomExpr e = error $ "Non-exhaustive pattern in genAtomExpr: " ++ show e
  
