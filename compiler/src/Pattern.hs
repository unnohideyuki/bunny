module Pattern where

import qualified PreDefined
import           Symbol
import qualified Typing

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

data Expression = Case Variable [Clause]
                | Fatbar Expression Expression
                | OtherExpression Typing.Expr
                | Error
                | Lambda [Variable] Expression
                deriving Show

type Variable = Id

data Clause = Clause Typing.Assump [Variable] Expression
            | DefaultClause Variable Expression {- for temporary fix (#t001) -}
            deriving Show

subst :: Expression -> Variable -> Variable -> Expression
subst expr vnew vold =
  let
    subst_var v | v == vold = vnew
                | otherwise = v

    subst_vs vs = fmap (\v -> subst_var v) vs

    subst_c (Clause c vs e) = Clause c (subst_vs vs) (subst e vnew vold)

    subst_cs cs = fmap (\c -> subst_c c) cs

    subst_expr e = Typing.vsubst e vnew vold
  in
   case expr of
     Case v cs         -> Case (subst_var v) (subst_cs cs)
     Fatbar e1 e2      -> Fatbar (subst e1 vnew vold) (subst e2 vnew vold)
     OtherExpression e -> OtherExpression (subst_expr e)

type Equation = ([Typing.Pat], Expression)

isVar :: Equation -> Bool
isVar (Typing.PVar _:_, _) = True
isVar _                    = False

isCon :: Equation -> Bool
isCon (Typing.PCon _ _:_, _) = True
isCon _                      = False

getCon :: Equation -> Typing.Assump
getCon (Typing.PCon a _:_, _) = a
getCon _                      = error $ "must not happen, getCon"

-- Note: Starting with "_" guarantees that will be treated as a local variable
mkVar :: String -> Int -> Variable
mkVar n k = "_" ++ n ++ ".U" ++ show k

partition :: Eq b => (a -> b) -> [a] -> [[a]]
partition _ [] = []
partition _ [x] = [[x]]
partition f (x:x':xs)
  | f x == f x' = tack x (partition f (x':xs))
  | otherwise   = [x] : partition f (x':xs)
  where
    tack x xss = (x : head xss) : tail xss

match _ k [] qs def = foldr Fatbar def [e | ([], e) <- qs]
match n k (u:us) qs def =
  let
    matchVarCon k us qs def
      | isVar (head qs) = matchVar k us qs def
      | isCon (head qs) = matchCon k us qs def
      | otherwise       = error $ "matchVarCon error: " ++ show (head qs)

    matchVar k (u:us) qs def =
      match n k us [(ps, subst e u v) | (Typing.PVar v:ps, e) <- qs] def

    matchCon k (u:us) qs def =
      Case u [matchClause c k (u:us) (choose c qs) def | c <- cs]
        where cs = constructors (getCon (head qs))

    matchClause c k (u:us) qs def =
      Clause c us' (match
                    n
                    (k + k)
                    (us' ++ us)
                    [(ps' ++ ps, e) | (Typing.PCon c ps':ps, e) <- qs]
                    def)
      where
        k' = arity c
        us' = [mkVar n (i+k)| i <- [1..k']]

    choose c qs = [q | q <-qs, (getCon q) `cequal` c]
      where
        (n Typing.:>: _) `cequal` (n' Typing.:>: _) = n == n'
  in
   foldr (matchVarCon k (u:us)) def (partition isVar qs)
