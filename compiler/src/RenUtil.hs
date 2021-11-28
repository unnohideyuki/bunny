module RenUtil where

import qualified Absyn                      as A
import           PreDefined
import           Symbol
import           Types
import           Typing

import           Control.Monad.State.Strict (State, get, put)
import           Data.Char                  (isUpper)
import qualified Data.Graph                 as G
import           Data.List.Split            (splitOn)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust, fromMaybe)
import qualified Data.Tree                  as T
import           Debug.Trace

aTrue :: A.Exp
aTrue  = A.VarExp $ Name "True" (0,0) True

aFalse :: A.Exp
aFalse  = A.VarExp $ Name "False" (0,0) True

aThen :: A.Exp
aThen  = A.VarExp $ Name ">>" (0,0) False

nNil :: Name
nNil = Name "[]" (0,0) True

aNil :: A.Exp
aNil  = A.VarExp nNil

aCons :: A.Exp
aCons  = A.VarExp $ Name ":" (0,0) True

aBind :: A.Exp
aBind  = A.VarExp $ Name ">>=" (0,0) False

data Level = Level { lvPrefix :: !Id
                   , lvDict   :: !(Table Id)
                   , lvNum    :: !Int
                   }
             deriving Show

fromModname :: Maybe Name -> Id
fromModname name = case name of
  Just n  -> origName n
  Nothing -> "Main"

initialLevel      :: Maybe Name -> Level
initialLevel n = Level { lvPrefix = fromModname n
                       , lvDict   = empty
                       , lvNum    = 0
                       }

data DictDef = DictDef{ ddId      :: Id
                      , ddMethods :: [Id]
                      , ddDecls   :: [A.ValueDecl]
                      , ddTDecls  :: [A.ValueDecl]
                      , ddTyvar   :: Id
                      }
              deriving Show

data Fixity = Fixity Assoc Int
            deriving (Show, Eq)

data Assoc = LeftAssoc | RightAssoc | NoAssoc
           deriving (Show, Eq)

-- Renaming Monad

data RnState = RnState { rnModid    :: !Id
                       , rnLvs      :: ![Level]
                       , rnTenv     :: !(Table Id)
                       , rnIfxenv   :: !(Table Fixity)
                       , rnCe       :: !ClassEnv
                       , rnCms      :: !Assumps
                       , rnKdict    :: !(Table Kind)
                       , rnCdicts   :: ![(Id, DictDef)]
                       , rnConsts   :: !ConstructorInfo
                       , rnTConsts  :: ![(Id, Type)]
                       , rnNum      :: !Int
                       , rnSyn      :: ![(A.Type, ([A.Type], A.Type))]
                       , rnIContext :: ![((Id, Id), Id)]
                       }
               deriving Show

type RN a = State RnState a

isSynonym :: A.Type -> RN Bool
isSynonym t = do
  d <- rnSyn <$> get
  case lookup t d of
    Just _  -> return True
    Nothing -> return False

getSynonym :: A.Type -> RN ([A.Type], A.Type)
getSynonym t = do
  d <- rnSyn <$> get
  return $ fromMaybe (error $ "getSynonym: synonym not found: " ++ show t)
                     (lookup t d)

actualType :: A.Type -> RN A.Type
actualType t = actual_type (remParTy t)
  where actual_type t@(A.AppTy t1 t2) = do
          let r = aAppTy t
          b <- case r of
            Just (c, _) -> isSynonym c
            Nothing     -> return False
          if b
            then do let Just (c, ts) = r
                    syn <- getSynonym c
                    let t' = substSyn syn ts
                    actual_type t'
            else do t1' <- actual_type t1
                    t2' <- actual_type t2
                    return (A.AppTy t1' t2')

        actual_type (A.FunTy t1 t2) = do
          t1' <- actual_type t1
          t2' <- actual_type t2
          return (A.FunTy t1' t2')

        actual_type (A.BangTy t) = do
          t' <- actual_type t
          return (A.BangTy t')

        actual_type (A.TupleTy ts) = do
          ts' <- mapM actual_type ts
          return (A.TupleTy ts')

        actual_type (A.ListTy t) = do
          t' <- actual_type t
          return (A.ListTy t')

        actual_type t@(A.Tycon _) = do
          b <- isSynonym t
          if b
            then do d <- rnSyn <$> get
                    case lookup t d of
                      Nothing -> return t
                      Just ([], t') -> actual_type t'
                      Just x -> error $ "actual_type: must not occur: " ++ show (t, x)
            else return t

        actual_type t = return t

getCDicts :: RN [(Id, DictDef)]
getCDicts = do
  st <- get
  return $ rnCdicts st

appendCDicts :: [DictDef] -> RN()
appendCDicts dicts= do
  st <- get
  let cdicts0 = rnCdicts st
  put st{rnCdicts = cdicts0 ++ [(ddId d, d) | d <-dicts]}

lookupCDicts :: Id -> RN DictDef
lookupCDicts n = do
  dicts <- getCDicts
  case lookup n dicts of
    Nothing   -> error $ "lookupCDicts: class not found: " ++ show (n, dicts)
    Just dict -> return dict

getLvs :: RN [Level]
getLvs = do st <- get
            return $ rnLvs st

putLvs :: [Level] -> RN ()
putLvs lvs = do st <-get
                put st{rnLvs=lvs}

pushLv :: Level -> RN ()
pushLv lv = do lvs <- getLvs
               putLvs (lv:lvs)

getPrefix :: RN Id
getPrefix = do x <- getLvs
               let lv = head x
               return $ lvPrefix lv

newNum :: RN Int
newNum = do
  x <- getLvs
  let lv = head x
      lvs = tail x
  let n = lvNum lv
  putLvs (lv{lvNum = n + 1} : lvs)
  return n

qname   :: Id -> RN Id
qname name = do
  maybe_qn <- maybeQname name
  return $ fromMaybe (error $ "qname not found: " ++ show name) maybe_qn

maybeQname   :: Id -> RN (Maybe Id)
maybeQname name = do
  lvs <- getLvs
  let r = findQName name lvs
  return $ if null r then Nothing else Just r
  where findQName n [] = ""
        findQName n (lv:lvs) =
          fromMaybe (findQName n lvs) (tabLookup n (lvDict lv))

getIfxenv :: RN (Table Fixity)
getIfxenv = do st <- get
               return $ rnIfxenv st

putIfxenv :: Table Fixity -> RN ()
putIfxenv ifxenv = do st <- get
                      put st{rnIfxenv=ifxenv}

renameVar :: Name -> RN Id
renameVar name
  | alreadyQualified (origName name) = return $ origName name
  | otherwise   = do
  x <- getLvs
  let lv = head x
      lvs = tail x
  let n = origName name
      n' = lvPrefix lv ++ "." ++ n
      dict' = insert n n' (lvDict lv)
      lv' = lv{lvDict=dict'}
  putLvs (lv':lvs)
  return n'
  where alreadyQualified s = s /= "." && length (splitOn "." s) > 1

regFixity :: A.Fixity -> Int -> [Name] -> RN ()
regFixity _ _ [] = return ()
regFixity f i (n:ns) = do reg (trFixity f i) n; regFixity f i ns
  where trFixity A.Infixl = Fixity LeftAssoc
        trFixity A.Infixr = Fixity RightAssoc
        trFixity A.Infix  = Fixity NoAssoc
        reg finfo name = do
          ifxenv <- getIfxenv
          qn <- renameVar name
          let  ifxenv' = insert qn finfo ifxenv
          if defined (tabLookup qn ifxenv)
            then error $ "duplicate fixity declaration:" ++ qn
            else putIfxenv ifxenv'

extrName :: A.Exp -> Name
extrName (A.VarExp name)       = name
extrName (A.FunAppExp f _)     = extrName f
extrName (A.InfixExp _ name _) = name
extrName (A.ParExp e)          = extrName e
extrName e                     = error $ "unexpected exp:" ++ show e

lookupInfixOp :: Name -> RN (Int, A.Fixity)
lookupInfixOp op = do
  maybe_op_qname <- maybeQname (origName op)
  case maybe_op_qname of
    Nothing -> return (9, A.Infixl)
    Just op_qname -> do
      st <- get
      case tabLookup op_qname (rnIfxenv st) of
        Just (Fixity LeftAssoc  x) -> return (x, A.Infixl)
        Just (Fixity RightAssoc x) -> return (x, A.Infixr)
        Just (Fixity NoAssoc    x) -> return (x, A.Infix)
        Nothing                    -> return (9, A.Infixl)

findCMs   :: Id -> RN (Maybe Assump)
findCMs qn = do
  st <- get
  return $ find' (rnCms st) qn
  where find' as qn = case Map.lookup qn as of
          Nothing -> Nothing
          Just sc -> Just (qn :>: sc)

appendCMs :: Assumps -> RN ()
appendCMs as = do
  st <- get
  put st{rnCms = Map.union (rnCms st) as}

enterNewLevelWith :: Id -> RN ()
enterNewLevelWith n = do
  currPrefix <- getPrefix
  let newPrefix = currPrefix ++ "." ++ n
      lv = Level newPrefix empty 0
  pushLv lv

enterNewLevel :: RN ()
enterNewLevel = do
  n <- newNum
  enterNewLevelWith ("l" ++ show n)

exitLevel :: RN ()
exitLevel = do
  st <- get
  let lvs' = tail $ rnLvs st
  put st{rnLvs=lvs'}

insertKdict :: Id -> Kind -> RN ()
insertKdict n k = do
  st <- get
  let kdict = rnKdict st
      kdict' = insert n k kdict
  put st{rnKdict=kdict'}

lookupKdict :: Id -> RN Kind
lookupKdict n = do
  st <- get
  case tabLookup n (rnKdict st) of
    Just k  -> return k
    Nothing -> error $ "kind not found: " ++ n

appendConstInfo :: [(Id, Int)] -> [(Id, [Assump])] -> RN ()
appendConstInfo da dc = do
  st <- get
  let ci = rnConsts st
      ci' = addConsts ci da dc
  put st{rnConsts = ci'}

lookupTConst :: Id -> RN (Maybe Type)
lookupTConst n = do
  st <- get
  let d = rnTConsts st
  return $ lookup n d

appendTConst :: Id -> Type -> RN ()
appendTConst n ty = do
  st <- get
  let d = rnTConsts st
  put st{rnTConsts = (n,ty):d}

--
-- A.Type utilities
--

-- remParTy -- strips parenthesis from the type
remParTy :: A.Type -> A.Type
remParTy (A.ParTy t)     = remParTy t -- A.ParTy is stripped
remParTy (A.FunTy t1 t2) = A.FunTy (remParTy t1) (remParTy t2)
remParTy (A.AppTy t1 t2) = A.AppTy (remParTy t1) (remParTy t2)
remParTy (A.BangTy t)    = A.BangTy (remParTy t)
remParTy (A.TupleTy ts)  = A.TupleTy $ map remParTy ts
remParTy (A.ListTy t)    = A.ListTy (remParTy t)
remParTy t               = t

-- aAppTy -- analize AppTy
--     If the type is the form: C T1 T2 ..
--     it returns               Just (C, [T1, T2 ..])
--     otherwise                Nothing
--
-- note: It strips parenthesis using remParTy and the caller doesn't have
--       to do it.
aAppTy :: A.Type -> Maybe (A.Type, [A.Type])
aAppTy t@(A.AppTy _ _) = aappty (remParTy t) []
  where aappty (A.AppTy c@(A.Tycon _) t) ts = Just (c, (t:ts))
        aappty (A.AppTy (A.Tyvar _) _) _ = Nothing
        aappty (A.AppTy t1@(A.AppTy _ _) t2) ts = aappty t1 (t2:ts)
        aappty t ts = error $ "aapty: " ++ show (t, ts)
aAppTy _ = Nothing

-- isTyvar -- checks if the type is Tyvar or not
isTyVar :: A.Type -> Bool
isTyVar (A.Tyvar _) = True
isTyVar _           = False

-- substSyn -- substitute the tyvars in the synonym
substSyn :: ([A.Type], A.Type) -> [A.Type] -> A.Type
substSyn (vs, t) ts = substsyn t
  where d = zip vs ts
        substsyn v@(A.Tyvar _)   = -- fromMaybe v (lookup v d)
          case lookup v d of
            Just v' -> v'
            Nothing -> trace (show (v, d)) v
        substsyn t@(A.Tycon _)   = t
        substsyn (A.FunTy t1 t2) = A.FunTy (substsyn t1) (substsyn t2)
        substsyn (A.AppTy t1 t2) = A.AppTy (substsyn t1) (substsyn t2)
        substsyn (A.BangTy t)    = A.BangTy (substsyn t)
        substsyn (A.TupleTy ts)  = A.TupleTy $ map substsyn ts
        substsyn (A.ListTy t)    = A.ListTy (substsyn t)
        substsyn (A.ParTy _)     = error $ "substsyn: must not occur"
        substsyn t@(A.RecTy _)   = t

tsortCDs :: [A.ClassDecl] -> [A.ClassDecl]
tsortCDs ds = let ty2IDs (A.AppTy (A.Tycon n) (A.Tyvar _)) = [origName n]
                  ty2IDs (A.TupleTy ts) = concatMap ty2IDs ts
                  ty2IDs (A.ParTy t) = ty2IDs t

                  d2id (A.ClassDecl (_, t) _) = head $ ty2IDs t

                  -- m: map from name to A.ClassDecl
                  m = Map.fromList $ zip (map d2id ds) ds

                  ks = Map.keys m
                  k2num = Map.fromList $ zip ks [0..]

                  d2edges (A.ClassDecl (Nothing, _) _) = []
                  d2edges (A.ClassDecl (Just s, t) _) =
                    let supers = ty2IDs s
                        ss = concatMap (\n -> case Map.lookup n k2num of
                                                Nothing -> []
                                                Just x  -> [x]) supers
                        i = fromJust $ Map.lookup (head $ ty2IDs t) k2num
                    in map (\j -> (i, j)) ss

                  edges = concatMap d2edges ds
                  g = G.buildG (0, length ks - 1) edges
                  -- TODO: when a SCC which size > 1 exists, report error
                  sortedNums = concatMap T.flatten $ G.scc g
                  sortedKeys = map (ks !!) sortedNums
                  ds' = map (\k -> fromJust $ Map.lookup k m) sortedKeys
  in ds'
