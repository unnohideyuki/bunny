----------------------------------------------------------------------------
-- Typing.hs
--
-- This Typing module is based on `Typing Haskell in Haskell', version of
-- November 23, 2000.  Copyright (c) Mark P Jones and the Oregon Graduate
-- Institute of Science and Technology, 1999-2000
--
-- This program is distributed as Free Software under the terms
-- in the file "License-Thih.txt" that is included in the distribution
-- of this software, copies of which may be obtained from:
--             http://www.cse.ogi.edu/~mpj/thih/
--
----------------------------------------------------------------------------
module Typing where

import           Symbol
import           Types

import           Control.Monad              (msum, zipWithM)
import           Control.Monad.State.Strict (State, get, put, runState, state)
import           Data.List                  (intersect, nub, partition, union,
                                             (\\))
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Debug.Trace

class HasKind t where
  kind :: t -> Kind
instance HasKind Tyvar where
  kind (Tyvar _ k) = k
instance HasKind Tycon where
  kind (Tycon _ k) = k
instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u)  = kind u
  kind (TAp t _) = case kind t of
    (Kfun _ k) -> k
    _          -> error "(kind t) must be Kfun."
  kind (TGen _)  = error "TGen must not occur here."

-- Substitutions

type Subst = [(Tyvar, Type)]

nullSubst :: Subst
nullSubst  = []

(+->)  :: Tyvar -> Type -> Subst
u +-> t = [(u, t)]

class Types t where
  apply :: Subst -> t -> t
  tv    :: t -> [Tyvar]

instance Types Type where
  apply s (TVar u)  = fromMaybe (TVar u) (lookup u s)
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply _ t         = t
  tv (TVar u)  = [u]
  tv (TAp l r) = tv l `union` tv r
  tv _         = []

instance Types a => Types [a] where
  apply s = map (apply s)
  tv      = nub.concatMap tv

infixr 4 @@
(@@)    :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge      :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1 ++ s2) else fail "merge fails"
  where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                    (map fst s1 `intersect` map fst s2)

-- Unification

mgu     :: Monad m => Type -> Type -> m Subst
varBind :: Monad m => Tyvar -> Type -> m Subst

mgu (TAp l r) (TAp l' r') = do s1 <- mgu l l'
                               s2 <- mgu (apply s1 r) (apply s1 r')
                               return (s2 @@ s1)
mgu (TVar u) t            = varBind u t
mgu t (TVar u)            = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
mgu t1 t2                 = fail $ "types do not unify: " ++ show (t1, t2)

varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = fail $ "occurs check fails: " ++ show u ++ ", " ++ show t
            | kind u /= kind t = fail "kinds do not match"
            | otherwise        = return (u +-> t)

match :: Monad m => Type -> Type -> m Subst

match (TAp l r) (TAp l' r')                    = do sl <- match l l'
                                                    sr <- match r r'
                                                    merge sl sr
match (TVar u) t            | kind u == kind t = return (u +-> t)
match (TCon tc1) (TCon tc2) | tc1 == tc2       = return nullSubst
match _ _                                      = fail "types do not match"

-- Predicates

data Qual t = [Pred] :=> t deriving (Eq, Show)

data Pred = IsIn Id Type deriving (Eq, Show)

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t)      = tv ps `union` tv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn _ t)      = tv t

mguPred, matchPred :: Monad m => Pred -> Pred -> m Subst
mguPred   = lift mgu
matchPred = lift match

lift :: Monad m => (Type -> Type -> m a) -> Pred -> Pred -> m a
lift m (IsIn i t) (IsIn i' t') | i == i'   = m t t'
                               | otherwise = fail "classes differ"

-- Class

type Class = ([Id], [Inst])
type Inst  = Qual Pred

-- Class Environment

data ClassEnv = ClassEnv { ceMap    :: Table Class
                         , defaults :: [Type]}
                deriving Show

classes :: Monad m => ClassEnv -> Id -> m Class
classes ce i = case tabLookup i (ceMap ce) of
                Just c  -> return c
                Nothing -> fail "class not defined"

super     :: ClassEnv -> Id -> [Id]
super ce i = case classes ce i of
  Just (is, _) -> is
  _            -> error $"super: " ++ i

insts     :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of
  Just (_, its) -> its
  _             -> error $ "insts: " ++ i

defined :: Maybe a -> Bool
defined (Just _) = True
defined Nothing  = False

modify       :: ClassEnv -> Id -> Class -> ClassEnv
modify ce@ClassEnv{ceMap = m} i c = ce{ceMap = insert i c m}

initialEnv :: ClassEnv
initialEnv  = ClassEnv { ceMap    = empty
                       , defaults = [tInteger]}

type EnvTransformer = ClassEnv -> Maybe ClassEnv

infixr 5 <:>
(<:>)       :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = f ce >>= g

addClass                           :: Id -> [Id] -> EnvTransformer
addClass i is ce
  | defined (classes ce i)          = fail "class already defined"
  | any (not.defined.classes ce) is = fail "superclass not defined"
  | otherwise                       = return (modify ce i (is, []))


addPreludeClasses :: EnvTransformer
addPreludeClasses  = addCoreClasses <:> addNumClasses

addCoreClasses :: EnvTransformer
addCoreClasses = addClass "Prelude.Eq" []
                 <:> addClass "Prelude.Ord" ["Prelude.Eq"]
                 <:> addClass "Prelude.Show" []
                 <:> addClass "Prelude.Read" []
                 <:> addClass "Prelude.Bounded" []
                 <:> addClass "Prelude.Enum" []
                 <:> addClass "Prelude.Functor" []
                 <:> addClass "Prelude.Monad" []

addNumClasses :: EnvTransformer
addNumClasses =
  addClass "Prelude.Num" ["Prelude.Eq", "Prelude.Show"]
  <:> addClass "Prelude.Real" ["Prelude.Num", "Prelude.Ord"]
  <:> addClass "Prelude.Fractional" ["Prelude.Num"]
  <:> addClass "Prelude.Integral" ["Prelude.Real", "Prelude.Enum"]
  <:> addClass "Prelude.RealFrac" ["Prelude.Real", "Prelude.Fractional"]
  <:> addClass "Prelude.Floating" ["Prelude.Fractional"]
  <:> addClass "Prelude.RealFloat" ["Prelude.RealFrac", "Prelude.Floating"]

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
  | not (defined (classes ce i)) = fail "no class for instance"
  | any (overlap p) qs           = fail "overlapping instance"
  | otherwise                    = return (modify ce i c)
  where its = insts ce i
        qs  = [q | (_ :=> q) <- its]
        c   = (super ce i, (ps :=> p) : its)

overlap    :: Pred -> Pred -> Bool
overlap p q = defined (mguPred p q)

exampleInsts :: EnvTransformer
exampleInsts  = addPreludeClasses

-------------------------------------------------------------------------------

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t)
  = p : concat [bySuper ce (IsIn i' t) | i' <- super ce i]

byInst                :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i _) = msum [tryInst it | it <- insts ce i]
  where tryInst (ps :=> h) = do u <- matchPred h p
                                Just  (map (apply u) ps)

entail        :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
                 case byInst ce p of
                   Nothing -> False
                   Just qs -> all (entail ce ps) qs

-------------------------------------------------------------------------------

inHnf           :: Pred -> Bool
inHnf (IsIn _ t) = hnf t
  where hnf (TVar _)   = True
        hnf (TCon _)   = False
        hnf (TAp t' _) = hnf t'
        hnf _          = error "must not happen in inHnf"

toHnfs      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | inHnf p   = return [p]
           | otherwise = case byInst ce p of
                           Nothing -> fail $ "context reduction: " ++ show p
                           Just ps -> toHnfs ce ps

simplify   :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where loop rs []                              = rs
        loop rs (p:ps) | entail ce (rs ++ ps) p = loop rs ps
                       | otherwise              = loop (p:rs) ps

reduce :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  return (simplify ce qs)

scEntail :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)

-------------------------------------------------------------------------------
-- Type Schemes

data Scheme = Forall [Kind] (Qual Type)
            deriving (Eq, Show)

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall _ qt)      = tv qt

quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where vs' = [v | v <- tv qt, v `elem` vs]
        ks  = map kind vs'
        s   = zip vs' (map TGen [0..])

toScheme  :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

-- Assumptions

data Assump = Id :>: Scheme deriving Show

type Assumps = Map.Map Id Scheme

instance Types Assump where
  apply s (i :>: sc) = i :>: apply s sc
  tv (_ :>: sc)      = tv sc

-- Types interface for Assumps
apply2As :: Subst -> Assumps -> Assumps
apply2As s as = let as' = map (\(i,sc) -> i :>: sc) (Map.toList as)
                    as'' = apply s as'
                in Map.fromList (map (\(i :>: sc) -> (i, sc)) as'')
tv2As :: Assumps -> [Tyvar]
tv2As as      = let as' = map (\(i,sc) -> i :>: sc) (Map.toList as)
                in tv as'

find                   :: Monad m => Id -> Assumps -> m Scheme
find i as = case Map.lookup i as of
              Nothing -> fail $ "unbound identifier: " ++ i
              Just sc -> return sc

-------------------------------------------------------------------------------
-- Type inference monad

type TI a = State (Subst, Int, Assumps) a

runTI :: (Subst, Int, Assumps) -> TI a -> a
runTI st ti = x where (x, _) = runState ti st

getSubst :: TI Subst
getSubst  = state $ \st@(s, _, _) -> (s, st)

unify      :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

extSubst   :: Subst -> TI ()
extSubst s' = state $ \(s, n, as) -> ((), (s'@@s, n, as))

enumId :: Int -> Id
enumId n = "v" ++ show n

newTVar :: Kind -> TI Type
newTVar k = state $ \(s, n, as) -> let v = Tyvar (enumId n) k
                                   in (TVar v, (s, n+1, as))

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

appendAssump :: Assumps -> TI ()
appendAssump as' = do
  (s, n, as) <- get
  put (s, n, Map.union as as')

getAssump :: TI Assumps
getAssump = do
  (_, _, as) <- get
  return as

class Instantiate t where
  inst :: [Type] -> t -> t
instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst _  t         = t
instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

-------------------------------------------------------------------------------
-- Type Inference Algorithm
-------------------------------------------------------------------------------
-- Infer

type Infer e t = ClassEnv -> Assumps -> e -> TI ([Pred], t)

-- Literals

data Literal = LitInt  Integer
             | LitChar Char
             | LitFrac Double
             | LitStr  String
             deriving Show

tiLit :: Literal -> TI ([Pred], Type)
tiLit (LitChar _) = return ([], tChar)
tiLit (LitInt _)  = do v <- newTVar Star
                       return ([IsIn "Prelude.Num" v], v)
tiLit (LitStr _) = return ([], tString)
tiLit (LitFrac _) = do v <- newTVar Star
                       return ([IsIn "Prelude.Fractional" v], v)
-- Patterns

data Pat = PVar Id
         | PWildcard
         | PAs Id Pat
         | PLit Literal
         | PCon Assump [Pat]
         deriving Show

tiPat :: Pat -> TI ([Pred], Assumps, Type)

tiPat (PVar i) = do v <- newTVar Star
                    return ([], Map.fromList [(i, toScheme v)], v)

tiPat PWildcard = do v <- newTVar Star
                     return ([], Map.empty, v)

tiPat (PAs i pat) = do (ps, as, t) <- tiPat pat
                       return (ps, Map.insert i (toScheme t) as, t)

tiPat (PLit l) = do (ps, t) <- tiLit l
                    return (ps, Map.empty, t)

tiPat (PCon (_ :>: sc) pats) = do (ps, as, ts) <- tiPats pats
                                  t'           <- newTVar Star
                                  (qs :=> t)   <- freshInst sc
                                  unify t (foldr fn t' ts)
                                  return (ps ++ qs, as, t')

tiPats     :: [Pat] -> TI([Pred], Assumps, [Type])
tiPats pats = do psasts <- mapM tiPat pats
                 let ps = concat [ps' | (ps', _, _) <- psasts]
                     as = Map.unions [as' | (_, as', _) <- psasts]
                     ts = [t | (_, _, t) <- psasts]
                 return (ps, as, ts)

-- Expressions

data Expr = Var   Id
          | Lit   Literal
          | Const Assump
          | Ap    Expr Expr
          | Let   BindGroup Expr
          deriving Show

tiExpr                       :: Infer Expr Type
tiExpr _ as (Var i)           = do sc         <- find i as
                                   (ps :=> t) <- freshInst sc
                                   return (ps, t)
tiExpr _ _ (Const (_ :>: sc)) = do (ps :=> t) <- freshInst sc
                                   return (ps, t)
tiExpr _ _ (Lit l)            = do (ps, t) <- tiLit l
                                   return (ps, t)
tiExpr ce as (Ap e f)         = do (ps, te) <- tiExpr ce as e
                                   (qs, tf) <- tiExpr ce as f
                                   t        <- newTVar Star
                                   unify (tf `fn` t) te
                                   return (ps ++ qs, t)
tiExpr ce as (Let bg e)       = do (ps, as') <- tiBindGroup ce as bg
                                   (qs, t)   <- tiExpr ce (Map.union as' as) e
                                   let as'' = qualifyAs ps as'
                                   appendAssump as''
                                   return (ps ++ qs, t)

{- The assumptions for local variable in Let expressions are
-- lacking qualifiers, that are added by qualifyAs.
-- It might be a temporary fix because i don't know the best way to do it.
-}
qualifyAs :: [Pred] -> Assumps -> Assumps
qualifyAs ps as = Map.fromList $ map (qualas ps) (Map.toList as)
  where
    qualas [] a = a
    qualas (p@(IsIn _ t1@(TVar v)):ps1) a@(n, Forall ks (ps2 :=> t2)) =
      if v `elem` tv t2 then
        qualas ps1 (n, Forall (kind t1:ks) ((p:ps2) :=> t2))
      else
        qualas ps1 a
    qualas _ _ = error "qualas:: unexpected"

-- Substitution (for variable, not for type vars), used from Pattern.hs
vsubst :: Expr -> Id -> Id -> Expr
vsubst var@(Var v) vnew vold | v == vold = Var vnew
                            | otherwise = var

vsubst lit@(Lit _) _ _ = lit

vsubst c@(Const _) _ _ = c

vsubst (Ap e1 e2) vnew vold = Ap (vsubst e1 vnew vold) (vsubst e2 vnew vold)

vsubst (Let bg e) vnew vold
  | isFree bg vold = Let (vsubst_bg bg vnew vold) (vsubst e vnew vold)
  | otherwise      = Let bg e
  where
    isFree (es, iss) n =
      notElem n $ fmap (\(s,_,_) -> s) es ++ fmap fst (concat iss)

    isFree_p (PVar s) n    = s /= n
    isFree_p (PAs s pat) n = (s /= n) && isFree_p pat n
    isFree_p (PCon _ ps) n = isFree_ps ps n
    isFree_p _ _           = True

    isFree_ps ps n = and $ fmap (`isFree_p` n) ps

    -- Caution: User must guaranntee that vnew is free in ps
    vsubst_alt alt@(ps, expr) vn vo
      | isFree_ps ps vo = (ps, vsubst expr vn vo)
      | otherwise       = alt

    vsubst_alts alts vn vo = fmap (\alt -> vsubst_alt alt vn vo) alts

    vsubst_bg :: BindGroup -> Id -> Id -> BindGroup
    vsubst_bg (es, iss) vn vo = (es', iss')
      where
        es' = fmap (\(n, sc, alts) -> (n, sc, vsubst_alts alts vn vo)) es
        iss' = fmap
               (fmap (\(n, alts) -> (n, vsubst_alts alts vn vo)))
               iss

-- Alternatives

type Alt = ([Pat], Expr)

tiAlt :: Infer Alt Type
tiAlt ce as (pats, e) = do (ps, as', ts) <- tiPats pats
                           (qs, t)       <- tiExpr ce (Map.union as' as) e
                           return (ps ++ qs, foldr fn t ts)

tiAlts             :: ClassEnv -> Assumps -> [Alt] -> Type -> TI [Pred]
tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
                         mapM_ (unify t . snd) psts
                         return (concatMap fst psts)

-------------------------------------------------------------------------------

split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred]
                      -> m ([Pred], [Pred])

split ce fs gs ps = do ps' <- reduce ce ps
                       let (ds, rs) = partition (all (`elem` fs) . tv) ps'
                       rs' <- defaultedPreds ce (fs ++ gs) rs
                       return (ds, rs \\ rs')

type Ambiguity = (Tyvar, [Pred])

ambiguities        :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities _ vs ps = [(v, filter (elem v . tv) ps) | v <- tv ps \\ vs]

numClasses :: [Id]
numClasses  = ["Prelude.Num", "Prelude.Integral", "Prelude.Floating",
               "Prelude.Fractional",
               "Prelude.Real", "Prelude.RealFloat", "Prelude.RealFrac"]

stdClasses :: [Id]
stdClasses  = [ "Prelude.Eq", "Prelude.Ord", "Prelude.Show"
              , "Prelude.Read", "Prelude.Bounded", "Prelude.Enum"
              , "Prelude.Ix", "Prelude.Functor"
              , "Prelude.Monad", "MonadPlus"] ++ numClasses

candidates           :: ClassEnv -> Ambiguity ->[Type]
candidates ce (v, qs) = [t' | let is = [i | IsIn i _ <- qs]
                                  ts = [t | IsIn _ t <- qs],
                              all (TVar v ==) ts,
                              any (`elem` numClasses) is,
                              all (`elem` stdClasses) is,
                              t' <- defaults ce,
                              all (entail ce []) [IsIn i t' | i<- is]
                            ]

withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a)
                  -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps
  | any null tss = fail $ "cannot resolve ambiguity: " ++ show vps
  | otherwise    = return (f vps (map head tss))
  where vps = ambiguities ce vs ps
        tss = map (candidates ce) vps

defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds  = withDefaults (\vps _ -> concatMap snd vps)

defaultSubst :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst  = withDefaults (\vps ts -> zip (map fst vps) ts)

-------------------------------------------------------------------------------

type Expl = (Id, Scheme, [Alt])

tiExpl :: ClassEnv -> Assumps -> Expl -> TI [Pred]
tiExpl ce as (_, sc, alts)
  = do (qs :=> t) <- freshInst sc
       ps         <- tiAlts ce as alts t
       s          <- getSubst
       let qs'     = apply s qs
           t'      = apply s t
           fs      = tv2As (apply2As s as)
           gs      = tv t' \\ fs
           sc'     = quantify gs (qs' :=> t')
           ps'     = filter (not.entail ce qs') (apply s ps)
       (ds, rs)   <- split ce fs gs ps'
       if sc /= sc' then
           fail "signature too general"
         else if not (null rs) then
           fail "context too weak"
         else
           return ds

-------------------------------------------------------------------------------

type Impl = (Id, [Alt])

restricted   :: [Impl] -> Bool
restricted = any simple
  where simple (_, alts) = any (null.fst) alts

tiImpls         :: Infer [Impl] Assumps
tiImpls ce as bs = do ts <- mapM (\_ -> newTVar Star) bs
                      let is    = map fst bs
                          scs   = map toScheme ts
                          as'   = Map.union (Map.fromList (zipWith (,) is scs)) as
                          altss = map snd bs
                      pss <- zipWithM (tiAlts ce as') altss ts
                      s   <- getSubst
                      let ps' = apply s (concat pss)
                          ts' = apply s ts
                          fs  = tv2As (apply2As s as)
                          vss = map tv ts'
                          gs  = foldr1 union vss \\ fs
                      (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
                      if False && restricted bs then
                        let gs'  = gs \\ tv rs
                            scs' = map (quantify gs' . ([]:=>)) ts'
                        in return (ds ++ rs, Map.fromList (zip is scs'))
                       else
                        let scs' = map (quantify gs . (rs:=>)) ts'
                        in return (ds, Map.fromList (zip is scs'))

-------------------------------------------------------------------------------

type BindGroup = ([Expl], [[Impl]])

tiBindGroup :: Infer BindGroup Assumps
tiBindGroup  ce as (es, iss) =
  do let as' = Map.fromList [(v, sc) | (v, sc, _) <- es]
     (ps, as'') <- tiSeq tiImpls ce (Map.union as' as) iss
     qss        <- mapM (tiExpl ce (Map.union as'' (Map.union as' as))) es
     return (ps ++ concat qss, Map.union as'' as')

tiSeq                  :: Infer bg Assumps -> Infer [bg] Assumps
tiSeq  _  _  _ []       = return ([], Map.empty)
tiSeq ti ce as (bs:bss) = do (ps, as')  <- ti ce as bs
                             (qs, as'') <- tiSeq ti ce (Map.union as' as) bss
                             return (ps ++ qs, Map.union as'' as')

-------------------------------------------------------------------------------
-- TIProg: Type Inference for Whole Programs

type Program = [BindGroup]

tiProgram ::
  ClassEnv -> Assumps -> [BindGroup] -> (Subst, Int, Assumps) -> Assumps
tiProgram ce as bgs cont = runTI cont $
                           do (ps, as') <- tiSeq tiBindGroup ce as bgs
                              s         <- getSubst
                              rs        <- reduce ce (apply s ps)
                              s'        <- defaultSubst ce [] rs
                              as''      <- getAssump
                              return (apply2As (s'@@s) (Map.union as' as''))

initialTI :: (Subst, Int, Assumps)
initialTI = (nullSubst, 0, Map.empty)

tiImportedProgram ::
  ClassEnv -> Assumps -> [BindGroup] -> (Subst, Int, Assumps)
  -> (Subst, Int, Assumps)
tiImportedProgram ce as bgs st =
  runTI st $ do (_, as') <- tiSeq tiBindGroup ce as bgs
                (s, n, _) <- get -- TODO: why not use [Assump] in the state
                return (s, n, Map.union as as')
