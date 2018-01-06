module BindGrouping where

import           Symbol
import           Types
import           Typing

import qualified Data.Graph as G
import           Data.List  (concatMap, (\\))
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Tree  as T

type TempBind = (Id, Maybe (Qual Type), [Alt])

toBg :: [TempBind] -> [BindGroup]
toBg tbs = [toBg2 tbs]

toBg2 :: [TempBind] -> BindGroup
toBg2 tbs =
  let
    (h, _, idx) = vars tbs
    deps = map tbDepend tbs

    -- Calculating SCC
    edges = concatMap (d2es h) deps
    g' = G.buildG (0, idx - 1) edges
    sccs = map T.flatten $ G.scc g'

    -- Preparation for TempBind->BindGroup translation
    scdict = collectTypes tbs
    bm = bindMap tbs h
  in
   scc2bg sccs bm scdict

vars :: [TempBind] -> (Map.Map Id Int, Map.Map Int Id, Int)
vars tbs' = vars' tbs' (Map.empty, Map.empty) 0
  where
    vars' [] (h, rh) i = (h, rh, i)
    vars' ((_,_,[]):tbs) (h, rh) i = vars' tbs (h, rh) i
    vars' ((n,_,_):tbs) (h, rh) i =
      case Map.lookup n h of
        Just _  -> vars' tbs (h, rh) i
        Nothing -> vars' tbs (Map.insert n i h, Map.insert i n rh) (i+1)

boundvars :: [Pat] -> [Id]
boundvars = concatMap boundvar

boundvar :: Pat -> [Id]
boundvar (PVar n)    = [n]
boundvar PWildcard   = []
boundvar (PAs n p)   = n : boundvar p
boundvar (PLit _)    = []
boundvar (PCon _ ps) = concatMap boundvar ps

evar :: Expr -> [Id]
evar (Var n) = [n]
evar (Lit _) = []
evar (Const _) = []
evar (Ap e1 e2) = evar e1 ++ evar e2
evar (Let bg@(es, iss) e) =
  let
    vs1 = evar e
    vs2 = fvFromBg bg
    bvs1 = map (\(n, _, _) -> n) es
    bvs2 = map fst (concat iss)

    vs = vs1 ++ vs2
    bounded = bvs1 ++ bvs2
  in
   vs \\ bounded

fvFromBg :: BindGroup -> [Id]
fvFromBg (es, iss) =
  let
    altsfv = concatMap fv
    vs1 = concatMap (\(_, _, alts) -> altsfv alts) es
    vs2 = concatMap (\(_, alts) -> altsfv alts) (concat iss)
  in
   vs1 ++ vs2

fv :: Alt -> [Id]
fv (ps, e) =
  let
    bvs = boundvars ps
    vs = evar e
  in
   vs \\ bvs

tbDepend :: TempBind -> (Id, [Id])
tbDepend (n, _, alts) = (n, concatMap fv alts)

d2es :: Map.Map Id Int -> (Id, [Id]) -> [G.Edge]
d2es h (n, vs) =
  let
    src = fromMaybe (error "d2es.src* must not occur") (Map.lookup n h)

    f v = case Map.lookup v h of
      Just i  -> [i]
      Nothing -> []

    dests = concatMap f vs
  in
   zip (repeat src) dests

collectTypes :: [TempBind] -> Map.Map Id Scheme
collectTypes tbs' = collty tbs' Map.empty
  where
    collty [] dict = dict
    collty ((name, Just qt, _):tbs) dict =
      let
        ts = tv qt
        scm = quantify ts qt
        dict' = Map.insert name scm dict
      in
       case Map.lookup name dict of
         Just x -> error $ "Duplicate type declaration: " ++ show (name, scm, x)
         Nothing -> collty tbs dict'
    collty ((_, Nothing, _):tbs) dict = collty tbs dict

bindMap :: [TempBind] -> Map.Map Id Int -> Map.Map Int TempBind
bindMap tbs' rh = bmap tbs' rh Map.empty
  where
    bmap [] _ d = d
    bmap ((_, _, []):tbs) h d = bmap tbs h d
    bmap (tb@(name, _, alts):tbs) h d =
      let
        i = fromMaybe (error $ "Must not happen. Vertex Id not found: " ++ name)
              (Map.lookup name h)
        tb' = case Map.lookup i d of
          Just (_, _, alts') -> (name, Nothing, alts++alts')
          Nothing            -> tb

        d' = Map.insert i tb' d
      in
       bmap tbs rh d'

scc2bg :: [[Int]] -> Map.Map Int TempBind -> Map.Map Id Scheme -> BindGroup
scc2bg sccs bm scdict = loop (reverse sccs) [] []
  where
    loop [] es iss = (es, iss)
    loop (c:cs) es iss =
      let
        (es', is') = cnvScc c [] []

        iss' = case is' of
          [] -> iss
          _  -> is':iss
      in
       loop cs (es++es') iss'

    cnvScc [] es is = (es, reverse is)
    cnvScc (x:xs) es is =
      let
        (name, alts) = case Map.lookup x bm of
          Just (name', _, alts') -> (name', alts')
          Nothing -> error $ "must not happen (cnvScc): " ++ show (x, bm)

        scm' = Map.lookup name scdict

        (es', is') = case scm' of
          Just scm -> ((name, scm, alts):es, is)
          Nothing  -> (es, (name, alts):is)
      in
       cnvScc xs es' is'
