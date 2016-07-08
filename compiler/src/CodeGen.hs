module CodeGen where

import STG
import Symbol

import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.List (intersperse)

import Debug.Trace

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

saveEnv :: GEN ()
saveEnv = do
  st <- get
  let cenv = env st
      es = estack st
  put st{estack=cenv:es}

restoreEnv :: GEN ()
restoreEnv = do
  st <- get
  let (e:es) = estack st
  put st{env=e, estack=es}

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

genExpr e@(LetExpr _ _) = genExpr' e False

genExpr e@(LamExpr vs expr)
  | fv e == [] = genLamExpr e
  | otherwise = error $ "LamExpr with free variables " ++ show e

genExpr e = error $ "Non-exaustive pattern in genExpr: " ++ show e

genExpr' (LetExpr bs e) delayed = do
  saveEnv
  rs <- genBs bs []
  addLocalVars rs
  sequence_ $ map (\(_, i, vs) -> setBoundVars i vs) rs
  (lamname, vs) <- genLambda e
  n <- nexti
  let s = "new LetExpr(null, new " ++ lamname ++ "())"
  appendCode $ "Expr t" ++ show n ++ " = " ++ s ++ ";"
  when (not delayed) $ setBoundVars n vs 
  restoreEnv
  return n
  where
    genBs [] rs = return rs
    genBs ((Bind (TermVar name) e):bs) rs
      | fv e == [] = do i <- genExpr e
                        genBs bs ((name, i, []):rs)
      | otherwise = do i <- genExpr' (LetExpr [] e) True
                       genBs bs ((name, i, fv e):rs)

    addLocalVars [] = return ()
    addLocalVars ((name, i, vs):rs) = do
      st <- get
      let cenv = env st
          cenv' = Map.insert name ("t" ++ show i) cenv
      put st{env=cenv'}
      addLocalVars rs

    setBoundVars _ [] = return ()
    setBoundVars n vs = do
      i <- nexti
      st <- get
      let s0 = "Expr[] t" ++ show i ++ " = {"
          cenv = env st
          n2v name = case Map.lookup name cenv of
            Just v -> v
            Nothing -> error $ "Variable not found: " ++ name
          s1 = concat $ intersperse "," $ map n2v vs
          s2 = "};"
      appendCode $ s0 ++ s1 ++ s2
      appendCode $ "((LetExpr)t" ++ show n ++ ").setEs(t" ++ show i ++ ");"

genLambda expr = do
  i <- nextgid
  let lamname = "LAM" ++ show i
      vs = fv expr
      ns = map (\i -> "args[" ++ show i ++ "]") [0..]
      nenv = fromList $ zip vs ns
      aty = length vs
  enterLambda aty lamname nenv
  n <- genExpr expr
  exitLambda n
  return (lamname, vs)

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
      (oenv:es) = estack st
      r = result st
      st' = st{ str = s
              , idx = i
              , env = oenv
              , sstack = ss
              , istack = is
              , estack = es
              , result = r ++ curs ++ "     }\n    }\n\n"
              }
  put st'

{- fv expr must be [] here -}
genLamExpr (LamExpr vs e) = do
  n <- nexti
  lamname <- genFBody vs e
  appendCode $
    "Expr t" ++ show n ++ " = RTLib.mkFun(new " ++ lamname ++ "());"
  return n

genFBody vs expr = do
  i <- nextgid
  let lamname = "LAM" ++ show i
      vs' = map (\(TermVar n) -> n) vs
      ns = map (\j -> "args[" ++ show j ++ "]") [0..]
      nenv = fromList $ zip vs' ns
      aty = length vs
  enterLambda aty lamname nenv
  n <- genExpr expr
  exitLambda n
  return lamname

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
  
