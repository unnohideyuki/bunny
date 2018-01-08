module CodeGen where

import           NameMangle
import           Semant                     (DictDef (..))
import           STG
import           Symbol

import           Control.Monad.State.Strict
import           Data.List                  (intercalate)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)

import           System.IO

emitPreamble :: Handle -> IO ()
emitPreamble h =
  let
     preamble = [ "import jp.ne.sakura.uhideyuki.brt.brtsyn.*;"
                , "import jp.ne.sakura.uhideyuki.brt.runtime.*;"
                , ""
                ]
     ploop []     = return ()
     ploop (s:ss) = do {hPutStrLn h s; ploop ss}
  in
   ploop preamble

emitProgram :: Program -> String -> String -> IO ()
emitProgram prog dest mname = do
  h <- openFile (dest ++ "/" ++ mname ++ ".java") WriteMode
  emitPreamble h
  emitHeader mname h -- Todo: module name.
  emitBinds prog h 0
  emitFooter h
  hClose h

emitBinds :: [Bind] -> Handle -> Int -> IO ()
emitBinds [] _ _ = return ()
emitBinds (b:bs) h n = do
  n' <- emitBind b h n
  emitBinds bs h n'

emitHeader :: Id -> Handle -> IO ()
emitHeader m h = hPutStrLn h $ "public class " ++ m ++ " {"

emitFooter :: Handle -> IO ()
emitFooter h = hPutStrLn h "}"

emitBind :: Bind -> Handle -> Int -> IO Int
emitBind b h n = do
  hPutStrLn h $ result st
  return n'
  where (n', st) = runState (genBind b) (initGenSt n)

genBind :: Bind -> GEN Int
genBind (Bind (TermVar n) e) = do
  enterBind n
  _ <- genBody e
  exitBind
  st <- get
  return $ gid st + 1
genBind _ = error "genBind: must not occur"

data GenSt = GenSt { str    :: String
                   , idx    :: Int
                   , env    :: Map.Map Id Id
                   , gid    :: Int
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
    name' = basenameM name

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


initGenSt :: Int -> GenSt
initGenSt n = GenSt { str = ""
                    , idx = 0
                    , env = Map.empty
                    , gid = n
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

-- Special case: overloaded function.
genExpr (FunAppExpr (FunAppExpr (AtomExpr (VarAtom (TermVar "#overloaded#"))) [e1]) [e2]) = do
  n <- nexti
  let (AtomExpr (VarAtom (TermVar m))) = e1
      bn = basenameM m
      lamname = "OL" ++ bn
      (AtomExpr (LitAtom (LitStr clsname))) = e2
  genOLlam lamname bn clsname
  appendCode $ "Expr t" ++ show n ++ " = RTLib.mkFun(new " ++ lamname ++ "());"
  return n
  where
    genOLlam lamname mname clsname = do
      enterLambda 1 lamname empty
      n <- nexti
      let dname = cls2dictNameM clsname
      appendCode $
        dname ++ " d = (" ++ dname ++ ") RTLib.extrDict(args[0]);"
      appendCode $ "Expr t" ++ show n ++ " = d.mk" ++ mname ++ "();"
      exitLambda n

genExpr (FunAppExpr f [e]) = do
  n1 <- genExpr f
  n2 <- genExpr e
  n <- nexti
  appendCode $
    "Expr t" ++ show n ++
    " = " ++ "RTLib.mkApp(t" ++ show n1 ++ ", t" ++ show n2 ++ ");"
  return n

genExpr (FunAppExpr f as) = do
  n1 <- genExpr f
  ns <- mapM genExpr as
  n2 <- nexti
  n3 <- nexti
  appendCode $
    "Expr[] t" ++ show n2 ++ " = {"
    ++ intercalate ", " (map (("t"++).show) ns) ++ "};"
  appendCode $
    "Expr t" ++ show n3 ++
    " = " ++ "RTLib.mkApp(t" ++ show n1 ++ ", t" ++ show n2 ++ ");"
  return n3

genExpr e@(LetExpr _ _) = genExpr' e False

genExpr e@(LamExpr _ _)
  | null (fv e) = genLamExpr e
  | otherwise = lamConv e >>= genExpr

genExpr (CaseExpr scrut alts') = do
  ns <- genExpr scrut
  na <- genalts alts' []
  n <- nexti
  let s = "new CaseExpr(t" ++ show ns ++ ", t" ++ show na ++ ")"
  appendCode $ "Expr t" ++ show n ++ " = " ++ s ++ ";"
  return n
  where
    genalts [] ts = do
      i <- nexti
      let s0 = "Alt[] t" ++ show i ++ " = {"
          s1 = intercalate "," (reverse ts)
          s2 = "};"
      appendCode $ s0 ++ s1 ++ s2
      return i
    genalts (CotrAlt name expr : alts) ts = do
      n <- genExpr expr
      i <- nexti
      let s0 = "Alt t" ++ show i ++ " = "
          s1 = "new CotrAlt(" ++ show name ++ ", t" ++ show n ++ ");"
          ts' = ("t" ++ show i) : ts
      appendCode $ s0 ++ s1
      genalts alts ts'
    genalts (DefaultAlt expr : alts) ts = do
      n <- genExpr expr
      i <- nexti
      let s0 = "Alt t" ++ show i ++ " = "
          s1 = "new DefaultAlt(t" ++ show n ++ ");"
          ts' = ("t" ++ show i) : ts
      appendCode $ s0 ++ s1
      genalts alts ts'

genExpr' :: Expr -> Bool -> GEN Int
genExpr' (LetExpr bs' e) delayed = do
  saveEnv
  rs <- genBs bs' []
  addLocalVars rs
  mapM_ (\(_, i, vs) -> setBoundVars i vs) rs
  (lamname, vs) <- genLambda e
  n <- nexti
  let s = "new LetExpr(null, new " ++ lamname ++ "())"
  appendCode $ "Expr t" ++ show n ++ " = " ++ s ++ ";"
  unless delayed $ setBoundVars n vs
  restoreEnv
  return n
  where
    genBs [] rs' = return rs'
    genBs (Bind (TermVar name) e' : bs) rs
      | null (fv e') = do i <- genExpr e'
                          genBs bs ((name, i, []):rs)
      | otherwise = do i <- genExpr' (LetExpr [] e') True
                       genBs bs ((name, i, fv e'):rs)
    genBs _ _ = error "genBs: must not occur"

    addLocalVars :: [(Id, Int, [Id])] -> GEN ()
    addLocalVars [] = return ()
    addLocalVars ((name, i, _):rs) = do
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
          n2v name = fromMaybe (error $ "Variable not found: " ++ name
                                ++ "\n, " ++ show cenv
                                ++ "\n, " ++ show (estack st))
                     (Map.lookup name cenv)
          s1 = intercalate "," $ map n2v vs
          s2 = "};"
      appendCode $ s0 ++ s1 ++ s2
      appendCode $ "((LetExpr)t" ++ show n ++ ").setEs(t" ++ show i ++ ");"

genExpr' _ _ = error "genExpr':: must not occur"

genLambda :: Expr -> GEN (Id, [Id])
genLambda expr = do
  i <- nextgid
  let lamname = "LAM" ++ show i
      vs = fv expr
      ns = map (\j -> "args[" ++ show j ++ "]") [(0::Int)..]
      nenv = fromList $ zip vs ns
      aty = length vs
  enterLambda aty lamname nenv
  n <- genExpr expr
  exitLambda n
  return (lamname, vs)

enterLambda :: Int -> Id -> Map.Map Id Id -> GEN ()
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

exitLambda :: Int -> GEN ()
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
genLamExpr :: Expr -> GEN Int
genLamExpr (LamExpr vs e) = do
  n <- nexti
  lamname <- genFBody vs e
  appendCode $
    "Expr t" ++ show n ++ " = RTLib.mkFun(new " ++ lamname ++ "());"
  return n
genLamExpr _ = error "genLamExpr: must not occur"

genFBody :: [Var] -> Expr -> GEN Id
genFBody vs expr = do
  i <- nextgid
  let lamname = "LAM" ++ show i
      vs' = map (\(TermVar n) -> n) vs
      ns = map (\j -> "args[" ++ show j ++ "]") [(0::Int)..]
      nenv = fromList $ zip vs' ns
      aty = length vs
  enterLambda aty lamname nenv
  n <- genExpr expr
  exitLambda n
  return lamname

lamConv :: Expr -> GEN Expr
lamConv e@(LamExpr vs expr) = do
  i <- nextgid
  let fvars = map TermVar $ fv e
      newv = TermVar $ "_X" ++ show i
      bs = [Bind newv (LamExpr (fvars ++ vs) expr)]
      v2e var = AtomExpr $ VarAtom var
      bd = FunAppExpr (v2e newv) $ map v2e fvars
  return $ LetExpr bs bd
lamConv _ = error "lamConv:: must not occur"

genAtomExpr :: Expr -> GEN Int
genAtomExpr (AtomExpr (VarAtom (TermVar n)))
  | n == "Prim.:"        = emit "RTLib.cons"
  | n == "Prim.[]"       = emit "RTLib.nil"
  | otherwise            = do
    st <- get
    let h = env st
        v = fromMaybe (refTopLevel n) (Map.lookup n h)
    emit v
  where
    emit s = do
      i <- nexti
      appendCode $ "Expr t" ++ show i ++ " = " ++ s ++ ";"
      return i

genAtomExpr (AtomExpr (VarAtom (DictVar n1 n2))) = do
  let s = cls2dictNameM $ n1 ++ "@" ++ n2
  n <- nexti
  appendCode $
    "Expr t" ++ show n ++
    " = (Expr) new AtomExpr(new Dict(new " ++ s ++ "()));"
  return n

genAtomExpr (AtomExpr (LitAtom (LitStr s))) = do
  n <- nexti
  appendCode $ "Expr t" ++ show n ++ " = RTLib.fromJString(" ++ show s ++ ");"
  return n

genAtomExpr (AtomExpr (LitAtom (LitChar c))) = do
  n <- nexti
  appendCode $ "Expr t" ++ show n ++ " = RTLib.fromChar(" ++ show c ++ ");"
  return n

genAtomExpr (AtomExpr (LitAtom (LitInt i))) = do
  n <- nexti
  appendCode $ "Expr t" ++ show n ++ " = RTLib.fromInteger(" ++ show i ++ ");"
  return n

genAtomExpr e = error $ "Non-exhaustive pattern in genAtomExpr: " ++ show e

refTopLevel :: Id -> Id
refTopLevel n =
  let
    m = modnameM  n
    n' = basenameM n
  in
   if '.' `notElem` n' -- ?? : todo clarify!
   then m ++ ".mk" ++ n' ++ "()"
   else error $ "Unbound variable " ++ n

emitDicts :: String -> [DictDef] -> IO ()
emitDicts _ [] = return ()
emitDicts dest (dict:ds) = do
  let dname = cls2dictNameM $ ddId dict
      msM = map mangle $ ddMethods dict
  h <- openFile (dest ++ "/" ++ dname ++ ".java") WriteMode
  emitPreamble h
  hPutStrLn h $
    "public abstract class " ++ dname ++ " extends Dictionary {"
  mapM_
    (\s -> hPutStrLn h $ "    abstract public Expr mk" ++ s ++ "();")
    msM
  hPutStrLn h "}"
  hClose h
  emitDicts dest ds

emitInsts :: String -> [DictDef] -> [(Id, Id)] -> IO ()
emitInsts _ _ [] = return ()
emitInsts dest dicts ((qin, qcn):ctab) = do
  let dicts' = dropWhile ((/= qcn).ddId) dicts
      ms = case dicts' of
        [] -> error $ "Class name not found: " ++ qcn
        _  -> ddMethods $ head dicts'
      msM = map mangle ms
      pdname = cls2dictNameM qcn
      dname = cls2dictNameM $ qin ++ "@" ++ qcn
      mname = modname qin
  h <- openFile (dest ++ "/" ++ dname ++ ".java") WriteMode
  emitPreamble h
  hPutStrLn h $ "public class " ++ dname ++ " extends " ++ pdname ++ "{"
  mapM_
    (\s -> do hPutStrLn h $ "    public Expr mk" ++ s ++ "(){"
              hPutStr h $ "      return " ++ mangle mname ++ "."
              hPutStr h $ "mk" ++ mangle ("I%" ++ basename qin ++ ".")
              hPutStrLn h $ s ++ "();"
              hPutStrLn h "    }")
    msM
  hPutStrLn h "}"
  hClose h
  emitInsts dest dicts ctab


