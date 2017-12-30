Code Walk Through (01): Semant.hs
=================================

econst :: Assump -> Expr
------------------------

Assump を受け取って、名前付き定数 (named constant) を表す 
Typing.Expr 型の値を返す。

名前付き定数とは、構築子や、抽象データ型のセレクタ関数、または、メンバ関数といったもの。

pNil :: Pat
-----------

[] のパタンを返す。

eNil :: Expr
------------

[] を表す Typing.Expr 型の値を返す。



Level -- α変化のためのレベル管理用データ
-------------------------------------

````Haskell
data Level = Level { lvPrefix :: !Id
                   , lvDict :: !(Table Id)
                   , lvNum :: !Int
                   }
````

lvPrefix : 処理中のレベルに当たる接頭辞。
           トップレベルでは、モジュール名を接頭辞にすることで
           ユニーク性を保証する。
           ※ いまの initialLevel (後述）実装では、このあたりが
              いいかげんかも。

lvDict : α変換のための辞書 (α変換前→α変換後、のはず）

lvNum : レベル番号。同じ階層にあるレベルに通し番号を振るための状態変数。











