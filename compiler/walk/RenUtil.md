RenUtil.hs
==========

RenUtil �� Rename ���W���[�����ŋ��ʂɗp������f�[�^�^�A�֐��A�萔�Ȃǂ��`���郂�W���[���ł���B

���O�_�ȂǁF
- ���W���[���� (RenUtil) �����܂����B�������O���v��������ς������B

## �萔

### aTrue, aFalse

````Haskell
aTrue :: A.Exp
aTrue  = A.VarExp $ Name "True" (0,0) True

aFalse :: A.Exp
aFalse  = A.VarExp $ Name "False" (0,0) True
````

True, False �ɂ�����A���ۍ\���̎��B
Typing ���W���[�����T�|�[�g���Ă��Ȃ��\���̂��߂ɁASemant �ł��ꕔ�E��������̂����A
�����ŗp������B�ړ����� `a` �� Absyn �̓������B

### aThen

````
aThen :: A.Exp
aThen  = A.VarExp $ Name ">>" (0,0) False
````

`>>` (then) �ɂ�����A���ۍ\���̎��Bdo �L�@�̒E���Ŏg�p���Ă���B

### nNil

````Haskell
nNil :: Name
nNil = Name "[]" (0,0) True
````

`[]` �ɂ����� Name �^�̒l�B���� `aNil` ��A`renIDictdefDecls` ���ŗp�����Ă���B

### aNil

````Haskell
aNil :: A.Exp
aNil  = A.VarExp nNil
````

`[]` �ɂ����钊�ۍ\���̎��B`renExp` �ŗp�����Ă���B

### aCons

````Haskell
aCons :: A.Exp
aCons  = A.VarExp $ Name ":" (0,0) True
````

`:` �ɂ����钊�ۍ\���̎��B`renExp` �ŗp�����Ă���B


### aBind

````Haskell
aBind :: A.Exp
aBind  = A.VarExp $ Name ">>=" (0,0) False
````

`>>=` �ɂ����钊�ۍ\���̎��B`renExp` �� `do` �L�@�̒E���̍ۂɗp�����Ă���B

## Level: �� �ϊ��̂��߂̃��x���Ǘ�

### Level

````Haskell
data Level = Level { lvPrefix :: !Id
                   , lvDict   :: !(Table Id)
                   , lvNum    :: !Int
                   }
             deriving Show
````

`Level` �́A�� �ϊ��ɕK�v�ȏ���ێ�����f�[�^�^�B

+ lvPrefix: �������̃��x���ɓ�����ړ����B�g�b�v���x���ł̓��W���[������ړ����ɂ��邱�ƂŃ��j�[�N����ۏ؁B
+ lvDict: �� �ϊ��̂��߂̎��� (���̖��O �� �� �ϊ���̖��O�j
+ lvNum: ���݂̔ԍ��B�����K�w�ɂ��郌�x���ɒʂ��ԍ���U�邽�߂̏�ԕϐ��B

���O�_�F

+ ���̖��O�́A�\�[�X�R�[�h���� qualifier ���w�肳��Ă��Ȃ������ꍇ�݂̂�z�肵�Ă���̂ł�
+ Bang pattern �̗��p�ɕ��j���Ȃ�

### fromModname

````Haskell
fromModname :: Maybe Name -> Id
fromModname name = case name of
  Just n  -> origName n
  Nothing -> "Main"
````

`Maybe Name` �^�̃��W���[��������A`Id` �^�̎��ʎq�����o�����߂̊֐��B
�w�肪�Ȃ������ꍇ (`Nothing` �������ꍇ�j�ɂ� "Main" ��Ԃ��B


### initialLevel

````Haskell
initialLevel      :: Maybe Name -> Level
initialLevel n = Level { lvPrefix = fromModname n
                       , lvDict   = empty
                       , lvNum    = 0
                       }
````

���W���[���� (`Maybe Name` �^�j�������ɂƂ�A���������ꂽ Level ��Ԃ��B

## �v���O�������̐錾�A��`��ێ�����f�[�^�^

### DictDef: ������`

````Haskell
data DictDef = DictDef{ ddId      :: Id
                      , ddMethods :: [Id]
                      , ddDecls   :: [A.Decl]
                      }
              deriving Show
````

�N���X�錾�̓��e��ێ�����f�[�^�^�B
�N���X�錾�́A�R�[�h�������ɂ́u�����v�̒�`�ɗp������B

### Fixity: ������

````Haskell
data Fixity = Fixity Assoc Int
            deriving (Show, Eq)

data Assoc = LeftAssoc | RightAssoc | NoAssoc
           deriving (Show, Eq)
````

�������錾�̏���ێ����邽�߂̃f�[�^�^�B�������A�E�����A�������̕ʂƁA�����̋����i�D�揇�ʁj
����Ȃ�B�D�揇�ʂ� 1 ���� 9 �̐����B

## Renaming Monad

### RnState, RN a

````Haskell
data RnState = RnState { rnModid  :: !Id
                       , rnLvs    :: ![Level]
                       , rnTenv   :: !(Table Id)
                       , rnIfxenv :: !(Table Fixity)
                       , rnCe     :: !ClassEnv
                       , rnCms    :: ![Assump]
                       , rnKdict  :: !(Table Kind)
                       , rnCdicts :: ![(Id, DictDef)]
                       }
               deriving Show

type RN a = State RnState a
````

�e�t�B�[���h�̐����F

- rnModid: ���l�[�����̃��W���[����
- rnLvs: Level �X�^�b�N
- rnTenv: 
- rnIfxenv: 
- rnCe: 
- rnCms: 
- rnKdict: 
- rnCdicts: 

````Haskell
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
````
