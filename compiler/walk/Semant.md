Code Walk Through (01): Semant.hs
=================================

## ユーティリティ関数

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

特に問題なし。

### aThen

````
aThen :: A.Exp
aThen  = A.VarExp $ Name ">>" (0,0) False
````

`>>` (then) にあたる、抽象構文の式。do 記法の脱糖で使用している。

懸念点：

一か所でしか用いられていない。

### nNil


````Haskell
nNil :: Name
nNil = Name "[]" (0,0) True
````

`[]` にあたる Name 型の値。つぎの `aNil` や、`renIDictdefDecls` 中で用いられている。

特に問題なし。

### aNil

````Haskell
aNil :: A.Exp
aNil  = A.VarExp nNil
````

`[]` にあたる抽象構文の式。`renExp` で用いられている。

懸念点：

一か所でしか用いられていない。

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

懸念点：

一か所でしか用いられていない。

## 結合性の表現

### FixtyInfo

````Haskell
data FixtyInfo = LeftAssoc  Int
               | RightAssoc Int
               | NoAssoc    Int
                 deriving (Show, Eq)
````

結合性宣言の情報を保持するためのデータ型。左結合、右結合、無結合の別と、結合の強さ（優先順位）
からなる。優先順位は 1 から 9 の整数。

懸念点：

* 「結合性」は、Fixity ではないか？

## α 変換のためのレベル管理

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
+ lvDict:α 変換のための辞書 (元の名前 → α 変換後の名前）
+ lvNum: レベル番号。同じ階層にあるレベルに通し番号を振るための状態変数。

懸念点：

+ 元の名前は、qualifier がない場合のみを想定しているのでは
+ Bang pattern の利用方針がない

### initialLevel

````Haskell
initialLevel      :: Maybe Name -> Level
initialLevel modid = Level { lvPrefix = case modid of
                                Just s  -> origName s
                                Nothing -> "Main"
                           , lvDict   = empty
                           , lvNum    = 0
                           }
````

初期化された Level を返す。

## Renaming Monad

### RnState, RN a

````Haskell
data RnState = RnState { rnModid    :: !Id
                       , rnLvs      :: ![Level]
                       , rnTenv     :: !(Table Id)
                       , rnIfxenv   :: !(Table FixtyInfo)
                       , rnCe       :: !ClassEnv
                       , rnCms      :: ![Assump]
                       , rnTbs      :: ![TempBind]
                       , rnTbsStack :: ![[TempBind]]
                       , rnKdict    :: !(Table Kind)
                       , rnCdicts   :: ![DictDef]
                       }
               deriving Show

type RN a = State RnState a
````

各フィールドの説明：

- rnModid: リネーム中のモジュール名
- rnLvs: 
- rnTenv: 
- rnIfxenv: 
- rnCe: 
- rnCms: 
- rnTbs: 
- rnTbsStack: 
- rnKdict: 
- rnCdicts: 










