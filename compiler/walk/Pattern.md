Pattern
=======

## パターンマッチ変換の方法

Pattern.hs で用いている方法について述べる。
これは、Efficient compilation of pattern-matching で述べられているものである。

### データ構造: match 

※match の構文と意味論をあとで追記

````Haskell
demo f  []     ys     = A f ys
demo f  (x:xs) []     = B f x xs
demo f  (x:xs) (y:ys) = C f x xs y ys
````

これは、match で以下のようにあらわされる

````
match [u1, u2, u3] 
      [([f, NIL,         ys          ), (A f ys)        ),
       ([f, (CONS x xs), NIL         ), (B f x xs)      ),
       ([f, (CONS x xs), (CONS y ys) ), (C f x xs y ys) ) ]
      ERROR
````
### The Variable Rule

次の例を考える。

````
match [u1, u2, u3] 
      [([f, NIL,         ys          ], (A f ys)        ),
       ([f, (CONS x xs), NIL         ], (B f x xs)      ),
       ([f, (CONS x xs), (CONS y ys) ], (C f x xs y ys) ) ]
      ERROR
````

すべての等式について、パターンの最初が変数である。

これは、最初の変数 u1 をとりのぞき、対応する形式変数 f も取り除き、
さらに、右辺式における f を u1 で置き換えることで以下のように変換できる。

````
match [u2, u3] 
      [([NIL,         ys          ], (A u1 ys)        ),
       ([(CONS x xs), NIL         ], (B u1 x xs)      ),
       ([(CONS x xs), (CONS y ys) ], (C u1 x xs y ys) ) ]
      ERROR
````

同じ方法は、各等式が変数で始まっていれば、変数名が等式ごとに異なっていても使える。

````
match [u2, u3]
      [([x, NIL      ], (B x)    ),
       ([y, CONS x xs], (C y x xs)]
      ERROR
````

↓

````
match [u3]
      [([NIL      ], (B u2)    ),
       ([CONS x xs], (C u2 x xs)]
      ERROR
````

※この例は、nodups の変換で登場します。

一般化すると The Variable Rule は次のようになります：

````
match (u:us)
      [((v1:psm), E1),
       ...
       ((vm:psm), Em) ]
      E
````

↓

````
match (us)
      [((psm), E1[u/v1]),
       ...
       ((psm), Em[u/vm]) ]
      E
````


### The Constructor Rule

前節の変形のつづきを考える。

````
match [u2, u3] 
      [([NIL,         ys          ], (A u1 ys)        ),
       ([(CONS x xs), NIL         ], (B u1 x xs)      ),
       ([(CONS x xs), (CONS y ys) ], (C u1 x xs y ys) ) ]
      ERROR
````

こんどは、どの等式のパターンもコンストラクタで始まっています。

これは、つぎの case 式と等しい：

````
case u2 of
  NIL        -> match [u3]
                ([ys          ], (A u1 ys)        )]
                ERROR
  CONS u4 u5 -> match [u4, u5, u3]
                ([x, xs, NIL         ], (B u1 x xs)      ),
                 [x, xs, (CONS y ys) ], (C u1 x xs y ys) ) ]
                ERROR
````


これは、同じコンストラクタから始まっている等式をまとめることで導かれます。

それぞれのグループでは、各コンストラクタフィールドに対応する変数が
新しく導入されます。
NILLにはフィールドがないので、新たな変数は不要。
CONSは２つのフィールドを持つので、２つの新しい変数 u4, u5 が導入されています。
これらの新しい変数は、元々のパターンに対応するサブパターンにマッチします。

もうひとつの例を見ておくのがいいかもしれません。

````
match [u1]
      [ ([NIL],                (A        ),
        ([CONS x NIL],         (B x)     ),
        ([CONS y (CONS x xs)], (C y x xs))]
      ERROR
````

これは、次のように reduce できます。

````
case u1 of
   NIL        -> match []
                       [ ([]), A)]
                       ERROR
   CONS u2 u3 -> match [u2, u3]
                       [([x, NIL],       (B x)     ),
                        ([y, CONS x xs], (C y x xs))]
                       ERROR
````

より一般的には、同じコンストラクタから始まる等式がとなりあっているとは、限りません。

こんなふうに：

````
match [u1]
      [ ([CONS x NIL],         (B x)     ),
　　　　([NIL],                (A        ),
        ([CONS y (CONS x xs)], (C y x xs))]
      ERROR
````

異なるコンストラクタから始まる２つの等式の順序をいれかえるのは、常に安全なので、
こういのは、あらかじめ次のように変形しておきます：

````
match [u1]
      [ ([NIL],                (A        ),
        ([CONS x NIL],         (B x)     ),
        ([CONS y (CONS x xs)], (C y x xs))]
      ERROR
````

パターンが網羅していない場合はどうか。たとえば、

````Haskell
last [x]        = x
last (y:(x:xs)) = last (x:xs)
````

は、次のような match 呼び出しになるのですが：

````
match [u1]
      [([CONS x NIL],         x                ),
       ([CONS y (CONS x xs)], (last (CONS x xs))]
      ERROR
````

これは、つぎのような case 式へと reduce できます。

````
case u1 of
  NIL        -> match [][] ERROR

  CONS u2 u3 -> match [u2, u3]
                      [([x, NIL]        x                ),
                       ([y, CONS x xs], (last (CONS x xs))]
                      ERROR
````

このケース式は、欠けていたコンストラクタと、空の match 呼び出し節を
含んでいなければなりません。
（なお、定義より、match [][] ERROR は ERROR と等しいことがわかります）

では、全ての等式のパタンがコンストラクタで始まる場合一般の
 reduction 規則について考えましょう。

型がひとつ決まれば、コンストラクタ c1,...,ck がわかります。
次に、等式は同じコンストラクタから始まるもの同士でグループわけ
することができます: qs1,...qsk 

ここで、qsi に含まれる等式は、いずれもコンストラクタ ci から始まります。
上の例の NIL のように、等式がないときには、対応するグループ qsi は
空のリストになります。

すると、match 呼び出しは次のような形になります。

````
match (u:us) (qs1 ++ ... ++ qsk) E
````

ここで、それぞれの qsi は、次のような形です：

````
 [ (((ci ps'i, 1), Ei,1)
   ...
   (((ci ps'i,mi), Ei,mi) ]
````

この match 呼び出しは、次の case 式へと reduce されます：

````
case u of
  c1 us'1 -> match (us'1 ++ us) qs'1 E
  ...
  ck us'k -> match (us'k ++ us) qs'k E
````

ただし、各 qs'i は次のような形で、

````
  [((ps'i,1 ++ psi,1), Ei,1),
   ...
   ((ps'i,mi ++ psi,mi), Ei,mi),]
````

各 us'i は新しい変数のリストです（ci の引数の数によって長さが決まる）。

このような書き方は、ややこしいので、あとで示すように関数型プログラミング言語で書いて
しまったほうが読みやすいでしょう。

繰り返しますが、ルールの正しさは、match の定義とパターンマッチの
意味論から証明できます。

### The Empty Rule

規則を適用していくと、変数リストが空であるような match にでくわすことが
あります。

````
match []
     [([], E1),
      ...
      ([], Em)]
     E
````

↓

````
E1 `fatbar` ... `fatbar` Em `fatbar` E
````

なお、E, ..., Em がいずれも FAIL でないことが保証できる場合には、
上の match は、m > 0 のとき E1, m = 0 のとき E となります。

### The Mixture Rule

これまでの規則で、ほとんど場合に関数定義をコンパイルできますが、
変数からはじまるものと、コンストラクタからはじまるものが混在する
ケースもあります。

````Haskell
demo' f []     ys     = A f ys
demo' f xs     []     = B f xs
demo' f (x:xs) (y:ys) = C f x xs y ys
````

これを match になおし、さらに変数ルールを適用したら、次のように
なります。

````
metch [u2, u3]
      [([NIL,       ys       ], (A u1 ys)       ),
       ([xs,        NIL      ], (B u1 xs)       ),
       ([CONS x xs, CONS y ys], (C u1 x xs y ys))]
      ERROR
````

こうなると、これまでの２規則はいずれも適用できません。
こういうときに、match の第3引数が役にたちます。

````
metch [u2, u3]
      [([NIL,       ys       ], (A u1 ys))]
      (match [u2, u3]
             [([xs,NIL], (B u1 xs))]
             (match [u2, u3]
             [([CONS x xs, CONS y ys], (C u1 x xs y ys))]
             ERROR))
````

つまり、等式をグループに分解したというわけです。

一般化：

````
match us qs E
````

における等式のリスト、qs を k 個のリストに分割します

```` 
 qs = qs1 ++ ... ++ qsk
````

この分割においては、各サブリストは、変数から始まるものだけか、
コンストラクタから始まるものだけになるようにします。

そうすると、以下のように変換できます。

````
match us qs1 (match us qs2 (.. (match us qsk E) ...))
````

## Haskell による実装

### モジュール宣言、インポート

````Haskell
module Pattern where

import qualified PreDefined
import           Symbol
import qualified Typing
````

Typing は qualified にしないほうがよかった気がする。

#### arity, constructors

現在は、組み込み型に関する arity, constructors をここでハードコーディングしてある。
要改善。

````Haskell
-- TODO: arity and constructors should not be defined here.
arity :: Typing.Assump -> Int
arity (c Typing.:>: _) =
  case c of
    "Prim.()"    -> 0
    "Prim.[]"    -> 0
    "Prim.:"     -> 2
    "Prim.True"  -> 0
    "Prim.False" -> 0
    "Prim.(,)"   -> 2
    _            -> error $ "unknown arity: " ++ c

constructors :: Typing.Assump -> [Typing.Assump]
constructors (c Typing.:>: _) =
  case c of
    "Prim.()"    -> [PreDefined.unitCfun]
    "Prim.[]"    -> [PreDefined.nilCfun, PreDefined.consCfun]
    "Prim.:"     -> [PreDefined.nilCfun, PreDefined.consCfun]
    "Prim.True"  -> [PreDefined.falseCfun, PreDefined.trueCfun]
    "Prim.False" -> [PreDefined.falseCfun, PreDefined.trueCfun]
    "Prim.(,)"   -> [PreDefined.pairCfun]
    _            -> error $ "unknown constructors: " ++ c
````

いずれも、The Constructor Rule に必要な情報を返す。

arity は、与えられたコンストラクタの引数の数をかえす。
constructors は、与えられたコンストラクタによって構築される型のすべてのコンストラクタを返す。

Typing.Expr におけるコンストラクタの表現が `Const Assump` であることに留意すると、
arity, constructors の型はいまのままでよさそう。

````Haskell
arity :: Assump -> Int
constructors :: Assump -> [Assump]
````

（constructors は、Assump -> [Expr] でもいいかもしれない。）



````Haskell
data Expression = Case Variable [Clause]
                | Fatbar Expression Expression
                | OtherExpression Typing.Expr
                | Error
                | Lambda [Variable] Expression
                deriving Show

type Variable = Id

data Clause = Clause Typing.Assump [Variable] Expression
            | DefaultClause Variable Expression -- TODO: temporary fix (#t001)
            deriving Show

subst :: Expression -> Variable -> Variable -> Expression
subst expr vnew vold =
  let
    subst_var v | v == vold = vnew
                | otherwise = v

    subst_c (Clause c vs e) = Clause c (fmap subst_var vs) (subst e vnew vold)
    subst_c _               = error "substC: must not occur"

    subst_expr e = Typing.vsubst e vnew vold
  in
   case expr of
     Case v cs         -> Case (subst_var v) (fmap subst_c cs)
     Fatbar e1 e2      -> Fatbar (subst e1 vnew vold) (subst e2 vnew vold)
     OtherExpression e -> OtherExpression (subst_expr e)
     _                 -> error "subst: must not occur"

type Equation = ([Typing.Pat], Expression)

isVar :: Equation -> Bool
isVar (Typing.PVar _:_, _) = True
isVar _                    = False

isCon :: Equation -> Bool
isCon (Typing.PCon _ _:_, _) = True
isCon _                      = False

getCon :: Equation -> Typing.Assump
getCon (Typing.PCon a _:_, _) = a
getCon _                      = error "getCon: must not occur"

-- Note: Starting with "_" guarantees that will be treated as a local variable
mkVar :: String -> Int -> Variable
mkVar n k = "_" ++ n ++ ".U" ++ show k

partition :: Eq b => (a -> b) -> [a] -> [[a]]
partition _ [] = []
partition _ [x] = [[x]]
partition f (x:x':xs) | f x == f x' = tack x (partition f (x':xs))
                      | otherwise   = [x] : partition f (x':xs)
  where
    tack y yss = (y : head yss) : tail yss

match :: Id -> Int -> [Variable] -> [Equation] -> Expression -> Expression

match _ _ [] qs def = foldr Fatbar def [e | ([], e) <- qs]
match n k' xs qs' def' =
  let
    matchVarCon k us qs def
      | isVar (head qs) = matchVar k us qs def
      | isCon (head qs) = matchCon k us qs def
      | otherwise       = error $ "matchVarCon error: " ++ show (head qs)

    matchVar k (u:us) qs def =
      match n k us [(ps, subst e u v) | (Typing.PVar v:ps, e) <- qs] def
    matchVar _ _ _ _ = error "matchVar: must not occur"

    matchCon k (u:us) qs def =
      Case u [matchClause c k (u:us) (choose c qs) def | c <- cs]
        where cs = constructors (getCon (head qs))
    matchCon _ _ _ _ = error "matchCon: must not occur"

    matchClause c k (_:us) qs def =
      Clause c us' (match
                    n
                    (k + k)
                    (us' ++ us)
                    [(ps' ++ ps, e) | (Typing.PCon _ ps':ps, e) <- qs]
                    def)
      where
        j = arity c
        us' = [mkVar n (i+k)| i <- [1..j]]
    matchClause _ _ _ _ _ = error "matchClouse: must not occur"

    choose c qs = [q | q <-qs, getCon q `cequal` c]
      where
        (i Typing.:>: _) `cequal` (n' Typing.:>: _) = i == n'
  in
   foldr (matchVarCon k' xs) def' (partition isVar qs')
````
