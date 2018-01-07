RenUtil.hs
==========

RenUtil は Rename モジュール内で共通に用いられるデータ型、関数、定数などを定義するモジュールである。

懸念点など：
- モジュール名 (RenUtil) がいまいち。いい名前を思いついたら変えたい。

## 定数

### aTrue, aFalse

````Haskell
aTrue :: A.Exp
aTrue  = A.VarExp $ Name "True" (0,0) True

aFalse :: A.Exp
aFalse  = A.VarExp $ Name "False" (0,0) True
````

True, False にあたる、抽象構文の式。
Typing モジュールがサポートしていない構文のために、Semant でも一部脱糖をするのだが、
そこで用いられる。接頭辞の `a` は Absyn の頭文字。

### aThen

````
aThen :: A.Exp
aThen  = A.VarExp $ Name ">>" (0,0) False
````

`>>` (then) にあたる、抽象構文の式。do 記法の脱糖で使用している。

### nNil

````Haskell
nNil :: Name
nNil = Name "[]" (0,0) True
````

`[]` にあたる Name 型の値。つぎの `aNil` や、`renIDictdefDecls` 中で用いられている。

### aNil

````Haskell
aNil :: A.Exp
aNil  = A.VarExp nNil
````

`[]` にあたる抽象構文の式。`renExp` で用いられている。

### aCons

````Haskell
aCons :: A.Exp
aCons  = A.VarExp $ Name ":" (0,0) True
````

`:` にあたる抽象構文の式。`renExp` で用いられている。


### aBind

````Haskell
aBind :: A.Exp
aBind  = A.VarExp $ Name ">>=" (0,0) False
````

`>>=` にあたる抽象構文の式。`renExp` で `do` 記法の脱糖の際に用いられている。

## Level: α 変換のためのレベル管理

### Level

````Haskell
data Level = Level { lvPrefix :: !Id
                   , lvDict   :: !(Table Id)
                   , lvNum    :: !Int
                   }
             deriving Show
````

`Level` は、α 変換に必要な情報を保持するデータ型。

+ lvPrefix: 処理中のレベルに当たる接頭辞。トップレベルではモジュール名を接頭辞にすることでユニーク性を保証。
+ lvDict: α 変換のための辞書 (元の名前 → α 変換後の名前）
+ lvNum: 現在の番号。同じ階層にあるレベルに通し番号を振るための状態変数。

懸念点：

+ 元の名前は、ソースコード中で qualifier が指定されていなかった場合のみを想定しているのでは
+ Bang pattern の利用に方針がない

### fromModname

````Haskell
fromModname :: Maybe Name -> Id
fromModname name = case name of
  Just n  -> origName n
  Nothing -> "Main"
````

`Maybe Name` 型のモジュール名から、`Id` 型の識別子を取り出すための関数。
指定がなかった場合 (`Nothing` だった場合）には "Main" を返す。


### initialLevel

````Haskell
initialLevel      :: Maybe Name -> Level
initialLevel n = Level { lvPrefix = fromModname n
                       , lvDict   = empty
                       , lvNum    = 0
                       }
````

モジュール名 (`Maybe Name` 型）を引数にとり、初期化された Level を返す。

## プログラム中の宣言、定義を保持するデータ型

### DictDef: 辞書定義

````Haskell
data DictDef = DictDef{ ddId      :: Id
                      , ddMethods :: [Id]
                      , ddDecls   :: [A.Decl]
                      }
              deriving Show
````

クラス宣言の内容を保持するデータ型。
クラス宣言は、コード生成時には「辞書」の定義に用いられる。

### Fixity: 結合性

````Haskell
data Fixity = Fixity Assoc Int
            deriving (Show, Eq)

data Assoc = LeftAssoc | RightAssoc | NoAssoc
           deriving (Show, Eq)
````

結合性宣言の情報を保持するためのデータ型。左結合、右結合、無結合の別と、結合の強さ（優先順位）
からなる。優先順位は 1 から 9 の整数。

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

各フィールドの説明：

- rnModid: リネーム中のモジュール名
- rnLvs: Level スタック
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
