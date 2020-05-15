module Pattern where

import           PreDefined
import           Symbol
import           Typing     (Assump (..), Expr (..), Pat (..), vsubst)

data Expression = Case Variable [Clause]
                | Fatbar Expression Expression
                | OtherExpression Typing.Expr
                | Error
                | Lambda [Variable] Expression
                deriving Show

type Variable = Id

data Clause = Clause Assump [Variable] Expression
            | DefaultClause Variable Expression -- TODO: temporary fix (#t001)
            deriving Show

subst :: Expression -> Variable -> Variable -> Expression
subst expr vnew vold =
  let
    subst_var v | v == vold = vnew
                | otherwise = v

    subst_c (Clause c vs e) = Clause c (fmap subst_var vs) (subst e vnew vold)
    subst_c _               = error "substC: must not occur"

    subst_expr e = vsubst e vnew vold
  in
   case expr of
     Case v cs         -> Case (subst_var v) (fmap subst_c cs)
     Fatbar e1 e2      -> Fatbar (subst e1 vnew vold) (subst e2 vnew vold)
     OtherExpression e -> OtherExpression (subst_expr e)
     _                 -> error "subst: must not occur"

type Equation = ([Pat], Expression)

isVar :: Equation -> Bool
isVar = not . isCon

isCon :: Equation -> Bool
isCon (PCon _ _:_, _) = True
isCon _               = False

getCon :: Equation -> Assump
getCon (PCon a _:_, _) = a
getCon x               = error $ "getCon: must not occur: " ++ show x

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

reduceMatch ::
  ConstructorInfo -> Id -> Int -> [Variable] -> [Equation] -> Expression
  -> Expression

reduceMatch ci n0 k0 vs0 qs0 def0 =
  let
    arity' = arity ci

    constructors' = constructors ci

    match :: Id -> Int -> [Variable] -> [Equation] -> Expression -> Expression

    match _ _ [] qs def = foldr Fatbar def [e | ([], e) <- qs]

    match n k' xs qs' def' =
      let
        matchVarCon k us qs def
          | isVar (head qs) = matchVar k us qs def
          | isCon (head qs) = matchCon k us qs def
          | otherwise       = error $ "matchVarCon error: " ++ show (head qs)

        matchVar k (u:us) qs def =
          match n k us [(ps, subst e u v) | (PVar v:ps, e) <- qs] def

        matchVar _ _ _ _ = error "matchVar: must not occur"

        matchCon k (u:us) qs def =
          Case u [matchClause c k (u:us) (choose c qs) def | c <- cs]
          where cs = constructors' (getCon (head qs))

        matchCon _ _ _ _ = error "matchCon: must not occur"

        matchClause c k (_:us) qs def =
          Clause c us' (match
                        n
                        (k + k)
                        (us' ++ us)
                        [(ps' ++ ps, e) | (PCon _ ps':ps, e) <- qs]
                        def)
          where
            j = arity' c
            us' = [mkVar n (i+k)| i <- [1..j]]

        matchClause _ _ _ _ _ = error "matchClouse: must not occur"

        choose c qs = [q | q <-qs, getCon q `cequal` c]
          where (i :>: _) `cequal` (n' :>: _) = i == n'
      in
        foldr (matchVarCon k' xs) def' (partition isVar qs')
  in
    match n0 k0 vs0 qs0 def0

