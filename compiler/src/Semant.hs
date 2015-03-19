module Semant where

import Control.Monad.State (State, state)
import Symbol
import qualified Absyn as A
import Typing
import PreDefined

import Debug.Trace

econst  :: Assump -> Expr
econst c = (Const c)

pNil :: Pat
pNil  = PCon nilCfun []

eNil :: Expr
eNil  = econst nilCfun

data FixtyInfo = LeftAssoc  Int
               | RightAssoc Int
               | NoAssoc    Int
                 deriving (Show, Eq)

data Level = Level { lv_prefix :: Id
                   , lv_dict   :: Table Id
                   , lv_num    :: Int
                   }
             deriving Show

initialLevel      :: Maybe Name -> Level
initialLevel modid = Level { lv_prefix = case modid of
                                Just s  -> orig_name s
                                Nothing -> "Main"
                           , lv_dict   = empty
                           , lv_num    = 0
                           }

-- Renaming Monad

data RnState = RnState { rn_modid  :: Id
                       , rn_lvs    :: [Level]
                       , rn_tenv   :: Table Id
                       , rn_ifxenv :: Table FixtyInfo
                       , rn_ce     :: ClassEnv
                       , rn_cms    :: [Assump]
                       }
               deriving Show

type RN a = State RnState a

renameVar :: Name -> RN ()
renameVar name = state $ \st@RnState{rn_lvs=(lv:lvs)} ->
  let
    prefix = lv_prefix lv
    qname = (++) (prefix ++ ".")
    n = orig_name name
    dict' = insert n (qname n) (lv_dict lv)
    lv' = lv{lv_dict=dict'}
  in
   ((), st{rn_lvs=(lv':lvs)})

regFixity :: A.Fixity -> Int -> [Name] -> RN ()
regFixity _ _ [] = return ()
regFixity fixity i (n:ns) = do reg (f i) n; regFixity fixity i ns
  where f = case fixity of
          A.Infixl -> LeftAssoc
          A.Infixr -> RightAssoc
          A.Infix  -> NoAssoc
        reg finfo name = state $ \st@RnState{rn_lvs=(lv:_), rn_ifxenv=ifxenv} ->
          let
            qn = (lv_prefix lv) ++ "." ++ (orig_name name)
            ifxenv' = insert qn finfo ifxenv
          in
           if defined (tabLookup qn ifxenv) then
             error $ "duplicate fixity declaration:" ++ qn
           else
             ((), st{rn_ifxenv=ifxenv'})

collectNames :: ([A.Decl], [A.Decl], [A.Decl]) -> [A.Decl]
                -> RN ([A.Decl], [A.Decl], [A.Decl])
collectNames x [] = return x
collectNames (ds, cds, ids) (decl:decls) = do
  (ds', cds', ids') <- collname decl
  collectNames (ds', cds', ids') decls
  where
    extrName (A.VarExp name)       = name
    extrName (A.FunAppExp f _)     = extrName f
    extrName (A.InfixExp _ name _) = name
    extrName e                     = error $ "unexpected exp:" ++ show e

    collname d@(A.ValDecl e _) = do renameVar (extrName e)
                                    return (ds ++ [d], cds, ids)
    collname (A.FixSigDecl fixity i ns) = do regFixity fixity i ns
                                             return (ds, cds, ids)

    collname d@(A.TypeSigDecl _ _)      = return (ds ++ [d], cds, ids)

    collname (A.DefaultDecl _)          = error "not yet: DefaultDecl"
    collname (A.ForeignDecl _)          = error "not yet: ForeignDecl"
    collname (A.SynonymDecl _ _)        = error "not yet: SynonymDecl"

    collname d@(A.ClassDecl _ _)        = return (ds, cds ++ [d], ids)
    collname d@(A.InstDecl _ _)         = return (ds, cds ++ [d], ids ++ [d])

    collname (A.DataDecl _ _ _)         = error "not yet: DataDecl"
    collname (A.NewtypeDecl _ _ _)      = error "not yet: NeytypeDecl"

type TempBinds = (Id, Maybe Scheme, [Alt])


transProg  :: A.Module -> RN ([TempBinds], [Assump])
transProg m = do
  let body = snd (A.body m)
  (ds, cds, ids) <- collectNames ([], [], []) body
  tbs <- transDecls [] ds
  return ([], [])

transDecls :: [TempBinds] -> [A.Decl] -> RN [TempBinds]
transDecls tbs [] = return tbs
transDecls tbs (d:ds) = do
  tb <- transDecl d
  trace (show (d, tb)) $ return [tb]
  -- trace (show (d, tb)) $ transDecls (tbs++[tb]) ds

transDecl (A.ValDecl exp rhs) = do
  (n, pats) <- transFExp exp
  rexp      <- transRhs  rhs
  return (n, Nothing, [(pats, rexp)])
transDecl _ = return ("", Nothing, [])

-- Todo:
transFExp (A.FunAppExp (A.VarExp n) (A.VarExp m)) = do
  qname_f <- qname (orig_name n)
  qname_p <- qname (orig_name m)
  a_pat   <- findCMs qname_p
  return (qname_f, [PCon a_pat []])

-- Todo:
transRhs (A.UnguardedRhs (A.VarExp n) []) = do
  qname_c <- qname (orig_name n)
  c_pat   <- findCMs qname_c
  return (Const c_pat)

qname   :: Id -> RN Id
qname name = state $ \st@RnState{rn_lvs=lvs} -> (findQName lvs name, st)
  where findQName [] n = error $ "qname not found: " ++ n
        findQName (lv:lvs) n = case tabLookup n (lv_dict lv) of
          Just qn -> qn
          Nothing -> findQName lvs n

findCMs   :: Id -> RN Assump
findCMs qn = state $ \st@RnState{rn_cms=as} -> (find' as qn, st)
  where find' [] qn' = error $ "Const/Member not found: " ++ qn'
        find' (a@(n :>: _):as') qn' | n == qn'   = a
                                    | otherwise = find' as' qn'
