module Typing where

import Data.List (nub, (\\), intersect, union, partition)
import Control.Monad (msum)
import Control.Monad.State (State, state)

import Types
import Symbol

class HasKind t where
  kind :: t -> Kind
instance HasKind Tyvar where
  kind (Tyvar _ k) = k
instance HasKind Tycon where
  kind (Tycon _ k) = k
instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u)  = kind u
  kind (TAp t _) = case (kind t) of
    (Kfun _ k) -> k
    _          -> undefined
  kind (TGen _)  = undefined

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
  apply s (TVar u)  = case lookup u s of
                        Just t  -> t
                        Nothing -> TVar u
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply _ t         = t

  tv (TVar u)  = [u]
  tv (TAp l r) = tv l `union` tv r
  tv _         = []

instance Types a => Types [a] where
  apply s = map (apply s)
  tv      = nub.concat.map tv

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
mgu _ _                   = fail "types do not unify"

varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = fail "occurs check fails"
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

data Qual t = [Pred] :=> t
            deriving (Eq, Show)

data Pred = IsIn Id Type
          deriving (Eq, Show)

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

data ClassEnv = ClassEnv { classes  :: Id -> Maybe Class
                         , defaults :: [Type]}

super     :: ClassEnv -> Id -> [Id]
super ce i = case classes ce i of Just (is, _) -> is
                                  _            -> undefined

insts     :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of Just (_, its) -> its
                                  _             -> undefined

defined :: Maybe a -> Bool
defined (Just _) = True
defined Nothing  = False

modify       :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce{classes = \j -> if i == j then Just c
                                             else classes ce j}

initialEnv :: ClassEnv
initialEnv  = ClassEnv { classes = \_ -> fail "class not defined"
                       , defaults = [tInteger, tDouble]}

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
addCoreClasses  = addClass "Eq" []
              <:> addClass "Ord" ["Eq"]
              <:> addClass "Show" []
              <:> addClass "Read" []
              <:> addClass "Bounded" []
              <:> addClass "Enum" []
              <:> addClass "Functor" []
              <:> addClass "Monad" []

addNumClasses :: EnvTransformer
addNumClasses  = addClass "Num" ["Eq", "Show"]
             <:> addClass "Real" ["Num", "Ord"]
             <:> addClass "Fractional" ["Num"]
             <:> addClass "Integral" ["Real", "Enum"]
             <:> addClass "RealFrac" ["Real", "Fractional"]
             <:> addClass "Floating" ["Fractional"]
             <:> addClass "RealFloat" ["RealFrac", "Floating"]

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
            <:> addInst [] (IsIn "Ord" tUnit)
            <:> addInst [] (IsIn "Ord" tChar)
            <:> addInst [] (IsIn "Ord" tInt)
            <:> addInst [ IsIn "Ord" (TVar (Tyvar "a" Star))
                        , IsIn "Ord" (TVar (Tyvar "b" Star))]
                        (IsIn "Ord" (pair (TVar (Tyvar "a" Star))
                                          (TVar (Tyvar "b" Star))))

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
        hnf _          = undefined

toHnfs      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | inHnf p   = return [p]
           | otherwise = case byInst ce p of
                           Nothing -> fail "context reduction"
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

data Assump = Id :>: Scheme

instance Types Assump where
  apply s (i :>: sc) = i :>: apply s sc
  tv (_ :>: sc)      = tv sc

find                   :: Monad m => Id -> [Assump] -> m Scheme
find i []               = fail $ "unboud identifier" ++ i
find i ((i' :>: sc):as) = if i == i' then return sc else find i as

-------------------------------------------------------------------------------
-- Type inference monad

type TI a = State (Subst, Int) a

getSubst :: TI Subst
getSubst  = state $ \st@(s, _) -> (s, st)

unify      :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

extSubst   :: Subst -> TI ()
extSubst s' = state $ \(s, n) -> ((), (s'@@s, n))

enumId :: Int -> Id
enumId n = ".v" ++ show n

newTVar :: Kind -> TI Type
newTVar k = state $ \(s, n) -> let v = Tyvar (enumId n) k
                               in (TVar v, (s, n+1))

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

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

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

-- Literals

data Literal = LitInt  Integer
             | LitChar Char
             | LitRat  Rational
             | LitStr  String

tiLit :: Literal -> TI ([Pred], Type)
tiLit (LitChar _) = return ([], tChar)
tiLit (LitInt _)  = do v <- newTVar Star
                       return ([IsIn "Num" v], v)
tiLit (LitStr _) = return ([], tString)
tiLit (LitRat _) = do v <- newTVar Star
                      return ([IsIn "Fractional" v], v)
