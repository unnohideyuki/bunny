module RenUtil where

import qualified Absyn                      as A
import           PreDefined
import           Symbol
import           Types
import           Typing

import           Control.Monad.State.Strict (State, get, put)
import           Data.Maybe                 (fromMaybe)

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
                      }
              deriving Show

data Fixity = Fixity Assoc Int
            deriving (Show, Eq)

data Assoc = LeftAssoc | RightAssoc | NoAssoc
           deriving (Show, Eq)

-- Renaming Monad

data RnState = RnState { rnModid   :: !Id
                       , rnLvs     :: ![Level]
                       , rnTenv    :: !(Table Id)
                       , rnIfxenv  :: !(Table Fixity)
                       , rnCe      :: !ClassEnv
                       , rnCms     :: ![Assump]
                       , rnKdict   :: !(Table Kind)
                       , rnCdicts  :: ![(Id, DictDef)]
                       , rnConsts  :: !ConstructorInfo
                       , rnTConsts :: ![(Id, Type)]
                       }
               deriving Show

type RN a = State RnState a

getCDicts :: RN [(Id, DictDef)]
getCDicts = do
  st <- get
  return $ rnCdicts st

putCDicts :: [DictDef] -> RN()
putCDicts dicts= do
  st <- get
  put st{rnCdicts = [(ddId d, d) | d <-dicts]}

lookupCDicts :: Id -> RN DictDef
lookupCDicts n = do
  dicts <- getCDicts
  case lookup n dicts of
    Nothing   -> fail $ "lookupCDicts: class not found: " ++ show (n, dicts)
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
getPrefix = do lv:_ <- getLvs
               return $ lvPrefix lv

newNum :: RN Int
newNum = do
  lv:lvs <- getLvs
  let n = lvNum lv
  putLvs (lv{lvNum = n + 1} : lvs)
  return n

qname   :: Id -> RN Id
qname name = do
  lvs <- getLvs
  return $ findQName name lvs
  where findQName n [] = error $ "qname not found: " ++ n
        findQName n (lv:lvs) =
          fromMaybe (findQName n lvs) (tabLookup n (lvDict lv))

getIfxenv :: RN (Table Fixity)
getIfxenv = do st <- get
               return $ rnIfxenv st

putIfxenv :: Table Fixity -> RN ()
putIfxenv ifxenv = do st <- get
                      put st{rnIfxenv=ifxenv}

renameVar :: Name -> RN Id
renameVar name = do
  lv:lvs <- getLvs
  let n = origName name
      n' = lvPrefix lv ++ "." ++ n
      dict' = insert n n' (lvDict lv)
      lv' = lv{lvDict=dict'}
  putLvs (lv':lvs)
  return n'

regFixity :: A.Fixity -> Int -> [Name] -> RN ()
regFixity _ _ [] = return ()
regFixity f i (n:ns) = do reg (trFixity f i) n; regFixity f i ns
  where trFixity A.Infixl = Fixity LeftAssoc
        trFixity A.Infixr = Fixity RightAssoc
        trFixity A.Infix  = Fixity NoAssoc
        reg finfo name = do
          (lv:_) <- getLvs
          ifxenv <- getIfxenv
          let qn = lvPrefix lv ++ "." ++ origName name
              ifxenv' = insert qn finfo ifxenv
          if defined (tabLookup qn ifxenv)
            then fail $ "duplicate fixity declaration:" ++ qn
            else putIfxenv ifxenv'

extrName :: A.Exp -> Name
extrName (A.VarExp name)       = name
extrName (A.FunAppExp f _)     = extrName f
extrName (A.InfixExp _ name _) = name
extrName e                     = error $ "unexpected exp:" ++ show e

lookupInfixOp :: Name -> RN (Int, A.Fixity)
lookupInfixOp op = do
  op_qname <- qname (origName op)
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
  where find' [] _ = Nothing
        find' (a@(n :>: _):as') qn' | n == qn'  = Just a
                                    | otherwise = find' as' qn'

appendCMs :: [Assump] -> RN ()
appendCMs as = do
  st <- get
  put st{rnCms = rnCms st ++ as}

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
