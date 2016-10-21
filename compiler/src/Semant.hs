module Semant where

import Data.List (foldl', (\\))
import Control.Monad (when)
import Control.Monad.State.Strict (State, state, get, put)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Graph as G
import qualified Data.Tree as T

import Debug.Trace

import Symbol
import qualified Absyn as A
import Types
import Typing
import PreDefined

econst  :: Assump -> Expr
econst c = (Const c)

pNil :: Pat
pNil  = PCon nilCfun []

eNil :: Expr
eNil  = econst nilCfun

aTrue :: A.Exp
aTrue  = A.VarExp $ Name "True" "" (0,0) True

aFalse :: A.Exp
aFalse  = A.VarExp $ Name "False" "" (0,0) True

aThen :: A.Exp
aThen  = A.VarExp $ Name ">>" "" (0,0) False

aNil :: A.Exp
aNil  = A.VarExp $ Name "[]" "" (0,0) True

aCons :: A.Exp
aCons  = A.VarExp $ Name ":" "" (0,0) True

aBind :: A.Exp
aBind  = A.VarExp $ Name ">>=" "" (0,0) False

data FixtyInfo = LeftAssoc  Int
               | RightAssoc Int
               | NoAssoc    Int
                 deriving (Show, Eq)

data Level = Level { lv_prefix :: !Id
                   , lv_dict   :: !(Table Id)
                   , lv_num    :: !Int
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

data RnState = RnState { rn_modid   :: !Id
                       , rn_lvs     :: !([Level])
                       , rn_tenv    :: !(Table Id)
                       , rn_ifxenv  :: !(Table FixtyInfo)
                       , rn_ce      :: !(ClassEnv)
                       , rn_cms     :: !([Assump])
                       , rn_tbs     :: !([TempBind])
                       , rn_tbstack :: !([[TempBind]])
                       , rn_kdict   :: !(Table Kind)
                       , rn_cdicts  :: !([DictDef])
                       }
               deriving Show

type RN a = State RnState a

putCDicts :: [DictDef] -> RN()
putCDicts dicts= do
  st <- get
  put st{rn_cdicts = dicts}

getCDicts :: RN [DictDef]
getCDicts = do
  st <- get
  return $ rn_cdicts st


lookupCDicts :: Id -> RN DictDef
lookupCDicts qcname = do
  dicts <- getCDicts
  let dicts' = dropWhile (\d -> qclsname d /= qcname) dicts
  case dicts' of
    [] -> error $ "lookupCDicts: class not found: " ++ show (qcname, dicts')
    _ -> return $ head dicts'

pushTbs :: RN ()
pushTbs = do
  st <- get
  let tbs = rn_tbs st
      tbstack = rn_tbstack st
  put st{rn_tbs=[], rn_tbstack=(tbs:tbstack)}

popTbs :: RN ()
popTbs = do
  st <- get
  let tbs = rn_tbs st
      (tbs', tbstack') = case rn_tbstack st of
        (x:xs) -> (x, xs)
        [] -> error "popTbs from empty stack."
  put st{rn_tbs=tbs', rn_tbstack=tbstack'}

renameVar :: Name -> RN Id
renameVar name = state $ \st@RnState{rn_lvs=(lv:lvs)} ->
  let
    prefix = lv_prefix lv
    n  = orig_name name
    n' = prefix ++ "." ++ n
    dict' = insert n n' (lv_dict lv)
    lv' = lv{lv_dict=dict'}
  in
   (n', st{rn_lvs=(lv':lvs)})

regFixity :: A.Fixity -> Int -> [Name] -> RN ()
regFixity _ _ [] = return ()
regFixity fixity i (n:ns) = do reg (f i) n; regFixity fixity i ns
  where f = case fixity of
          A.Infixl -> LeftAssoc
          A.Infixr -> RightAssoc
          A.Infix  -> NoAssoc
        reg finfo name = state $ \st@RnState{rn_lvs=(lv:lvs), rn_ifxenv=ifxenv} ->
          let
            qn = (lv_prefix lv) ++ "." ++ (orig_name name)
            ifxenv' = insert qn finfo ifxenv
          in
           if defined (tabLookup qn ifxenv) then
             error $ "duplicate fixity declaration:" ++ qn
           else
             ((), st{rn_ifxenv=ifxenv'})

extrName :: A.Exp -> Name
extrName (A.VarExp name)       = name
extrName (A.FunAppExp f _)     = extrName f
extrName (A.InfixExp _ name _) = name
extrName e                     = error $ "unexpected exp:" ++ show e

collectNames :: ([A.Decl], [A.Decl], [A.Decl]) -> [A.Decl]
                -> RN ([A.Decl], [A.Decl], [A.Decl])
collectNames x [] = return x
collectNames (ds, cds, ids) (decl:decls) = do
  (ds', cds', ids') <- collname decl
  collectNames (ds', cds', ids') decls
  where
    collname d@(A.ValDecl e _) = do _ <- renameVar (extrName e)
                                    return (ds ++ [d], cds, ids)
    collname (A.FixSigDecl fixity i ns) = do regFixity fixity i ns
                                             return (ds, cds, ids)

    collname d@(A.TypeSigDecl _ _)      = return (ds ++ [d], cds, ids)

    collname (A.DefaultDecl _)          = error "not yet: DefaultDecl"
    collname (A.ForeignDecl _)          = error "not yet: ForeignDecl"
    collname (A.SynonymDecl _ _)        = error "not yet: SynonymDecl"


    collname d@(A.ClassDecl (_, A.AppTy (A.Tycon n) _) ds') = do
      _ <- renameVar n
      mapM_ colname' ds'
      return (ds, cds ++ [d], ids)
      where colname' (A.ValDecl e _) = do _ <- renameVar (extrName e)
                                          return ()
            colname' _ = return ()


    collname d@(A.InstDecl _ _ _)       = return (ds, cds, ids ++ [d])

    collname (A.DataDecl _ _ _)         = error "not yet: DataDecl"
    collname (A.NewtypeDecl _ _ _)      = error "not yet: NewtypeDecl"

type TempBind = (Id, Maybe (Qual Type), [Alt])

data DictDef = DictDef{ qclsname :: Id
                      , methods :: [Id]
                      , decls :: [A.Decl]
                      }
              deriving Show

cdecl2dict :: Id -> A.Decl -> DictDef
cdecl2dict modid (A.ClassDecl (_, (A.AppTy (A.Tycon n) _)) ds) =
  let
    extrId Name{orig_name=s} = s

    name = modid ++ "." ++ extrId n

    extrMName (A.TypeSigDecl ns _) = map extrId ns
    extrMName _ = []

    ms = concatMap extrMName ds

    extrValDecl d@(A.ValDecl _ _) = [d]
    extrValDecl _ = []

    vdcls = concatMap extrValDecl ds
  in
   DictDef{qclsname = name, methods = ms, decls = vdcls}


renProg  :: A.Module -> RN ([BindGroup], [Assump], [DictDef], [(Id, Id)])
renProg m = do
  let body = snd (A.body m)
      modid = case A.modid m of
        Just (Name{orig_name=s}) -> s
        Nothing -> "Main"
  (ds, cds, ids) <- collectNames ([], [], []) body
  ctbs <- renCDecls cds []
  let bgs' = toBg ctbs
      as2 = map (\(n, scm, _) -> n :>: scm) $ fst $ head bgs'
  let dicts = map (cdecl2dict modid) cds
  putCDicts dicts
  (itbs, ctab) <- renIDecls ids
  tbs <- renDecls ds
  -- NOTE#1: followings are not clear! see the note page 233.
  let bgs = toBg $ tbs ++ itbs
      bgs'' = toBg $ ctbs ++ tbs ++ itbs
  st <- get
  let ce = rn_ce st
      as = rn_cms st
      as' = tiProgram ce (as ++ as2) bgs
  return (bgs'', as' ++ as2, dicts, ctab)


renCDecls :: [A.Decl] -> [TempBind] -> RN [TempBind]
renCDecls [] tbs = return tbs
renCDecls ((A.ClassDecl cls ds):cds) tbs = do
  clsadd cls
  let ds' = suppDs ds "Main.Monad" -- todo: have to extract from cls
  tbs' <- renDecls $ addvar cls ds'
  renCDecls cds (tbs ++ tbs')
  where clsadd (ctx, (A.AppTy (A.Tycon n) _)) = do
          cname <- qname $ orig_name n
          st <- get
          let ce = rn_ce st
              ce' = case addClass cname [] ce of -- todo: super class
                Just x -> x
                Nothing -> error $ "addClass failed: " ++ (show (cname, ce))
          put $ st{rn_ce=ce'}
          return ()
        addvar cls ds = let (_, sigvar) = cls
                            f (A.TypeSigDecl ns (_, sigdoc)) =
                              A.TypeSigDecl ns (Just sigvar, sigdoc)
                            f d = d
                        in map f ds

{- suppDs -- Exstract TypeSigDecls and supplement ValDecls that defines
             overloaded functions.
-}
suppDs :: [A.Decl] -> String -> [A.Decl]
suppDs ds clsname =
  let
    extrId (Name{orig_name=s}) = s

    ubNames [] cns cds = (cns, cds)

    ubNames (cd@(A.TypeSigDecl ns _):ds) cns cds =
      let
        ns' = map extrId ns
      in
       ubNames ds (cns ++ ns') (cd : cds)

    ubNames ((A.ValDecl e _):ds) cns cds = ubNames ds cns cds

    (ns, cds) = ubNames ds [] []

    mkv n = A.VarExp $
            Name{orig_name=n, qual_name="", name_pos=(-1, -1), isConName=False}
    mkoldcl n = A.ValDecl (mkv n) (A.UnguardedRhs
                                   (A.FunAppExp
                                    (A.FunAppExp (mkv "#overloaded#") (mkv n))
                                    (A.LitExp (A.LitString clsname undefined))
                                   )
                                   [])

    ds' = map mkoldcl ns
  in
   cds ++ ds'

renIDecls :: [A.Decl] -> RN ([TempBind], [(Id, Id)])
renIDecls [] = return ([], [])
renIDecls ((A.InstDecl ctx t ds):ids) = do
  (qcn, qin, i, a) <- case t of
    (A.AppTy (A.Tycon n1) (A.Tycon n2)) -> do qcn <- qname $ orig_name n1
                                              qin <- renameVar n2
                                              return (qcn, qin, n2, "")
    (A.AppTy (A.Tycon n1) (A.ListTy (A.Tyvar n2))) -> do
      qcn <- qname $ orig_name n1
      qin <- renameVar (Name {orig_name="[]"})
      return (qcn, qin, (Name {orig_name="[]"}), orig_name n2)
      
    _ -> error $ "Non-exhaustive pattern in case: " ++ show t
  
  k <- lookupKdict qcn
  let p = case orig_name i of
        "[]" -> (IsIn qcn (TAp (TCon (Tycon qin k)) (TVar (Tyvar a Star))))
        _ -> (IsIn qcn (TCon (Tycon qin k)))

  ps <- tops ctx
  instAdd ps p

  dict <- lookupCDicts qcn
  let defds = decls dict
      ds' = mergeDs ds defds
  enterNewLevelWith $ "I%" ++ (orig_name i) -- see STG/isLocal
  (ds'', _, _) <- collectNames ([], [], []) ds'
  tbs <- renDecls ds''
  exitLevel

  (tbs', ctab') <- renIDecls ids
  return $ (tbs ++ tbs', (qin, qcn):ctab')
  where instAdd ps p = do
          st <- get
          let ce = rn_ce st
              ce' = case addInst ps p ce of
                Just x -> x
                Nothing -> error $ "addInst failed: " ++ show (p, ce)
          put $ st{rn_ce=ce'}


        extrId' (A.ValDecl e _) = orig_name $ extrName e

        mergeDs dcls defdecls =
          let
            names = map extrId' dcls
            ds2 = filter (\d -> not $ elem (extrId' d) names) defdecls
          in
           dcls ++ ds2

        tops Nothing = return []
        tops (Just t) = tops' t

        tops' (A.ParTy t) = tops' t
        tops' (A.AppTy (A.Tycon n1) (A.Tyvar n2)) = do
          qcn <- qname $ orig_name n1
          let t = TVar $ Tyvar (orig_name n2) Star -- todo: always works?
          return [IsIn qcn t]
           
           
lookupKdict :: Id -> RN Kind
lookupKdict n = do
  st <- get
  case tabLookup n (rn_kdict st) of
    Just k -> return k
    Nothing -> error $ "kind not found: " ++ n

renDecls :: [A.Decl] -> RN [TempBind]
renDecls [] = do
  st <- get
  put st{rn_tbs=[]}
  return $ rn_tbs st
renDecls (d:ds) = do
  ntbs <- renDecl d
  st <- get
  let tbs = rn_tbs st
      tbs' = tbs++ntbs
  put st{rn_tbs=tbs'}
  renDecls ds

renDecl :: A.Decl -> RN [TempBind]
renDecl (A.ValDecl expr rhs) = do
  enterNewLevel
  (n, pats) <- renFExp expr
  rexp      <- renRhs  rhs
  exitLevel
  return [(n, Nothing, [(pats, rexp)])]

renDecl (A.TypeSigDecl ns (maybe_sigvar, sigdoc)) = do
  ns' <- mapM renameVar ns
  let kdict = kiExpr sigdoc []
  kdict `seq` return ()
  ps <- renSigvar maybe_sigvar kdict
  t <- renSigdoc sigdoc kdict
  t `seq` return [(n, Just (ps :=> t), []) | n <- ns']

renDecl decl = return [("", Nothing, [])]

kiExpr :: A.Type -> [(Id, Kind)] -> [(Id, Kind)]
kiExpr (A.FunTy t1 t2) dict =
  let dict' = kiExpr t1 dict
  in kiExpr t2 dict'
kiExpr (A.AppTy t1 t2) dict = dict ++ [(extrid t1, Kfun Star Star)
                                      ,(extrid t2, Star)]
  where extrid (A.Tyvar n) = orig_name n
kiExpr (A.Tyvar n) dict = dict ++ [(orig_name n, Star)]
kiExpr (A.ParTy e) dict = kiExpr e dict
kiExpr (A.Tycon _) dict = dict
kiExpr (A.ListTy e) dict = kiExpr e dict
kiExpr t dict = error $ "kiExpr: " ++ show (t, dict)

insertKdict :: Id -> Kind -> RN ()
insertKdict n k = do
  st <- get
  let kdict = rn_kdict st
      kdict' = insert n k kdict
  put st{rn_kdict=kdict'}

renSigvar :: Maybe A.Type -> [(Id, Kind)] -> RN [Pred]
renSigvar Nothing _ = return []
renSigvar (Just (A.AppTy (A.Tycon n) (A.Tyvar m))) kdict = do
  qn <- qname $ orig_name n
  let vname = orig_name m
      k = kindLookup vname kdict
  when (isConName n) (insertKdict qn k)
  return [IsIn qn (TVar (Tyvar vname k))]

renSigvar _ _ = error "renSigvar"

renSigdoc :: A.Type -> [(Id, Kind)] -> RN Type
renSigdoc (A.FunTy e1 e2) kdict = do
  t1 <- renSigdoc e1 kdict
  t2 <- renSigdoc e2 kdict
  return (t1 `fn` t2)
renSigdoc (A.Tyvar n) kdict = let vname = orig_name n
                                  k = kindLookup vname kdict
                              in return (TVar (Tyvar vname k))
renSigdoc (A.AppTy e1 e2) kdict = do
  t1 <- renSigdoc e1 kdict
  t2 <- renSigdoc e2 kdict
  return (TAp t1 t2)

renSigdoc (A.ParTy e) kdict = renSigdoc e kdict

-- TODO: should be fix this hard coding.
renSigdoc (A.Tycon n) _ = case orig_name n of
  "Integer" -> return tInteger
  "Int" -> return tInt
  "String" -> return tString
  "IO" -> return $ TCon (Tycon "IO" (Kfun Star Star))
  "()" -> return tUnit
  "Bool" -> return tBool
  s -> error $ "renSigDoc $ A.Tycon " ++ s

renSigdoc (A.ListTy e) kdict = do
  t <- renSigdoc e kdict
  return $ list t

renSigdoc t kdict = error $ "renSigdoc" ++ show t

kindLookup n kdict = case lookup n kdict of
  Just k -> k
  Nothing -> error $ "Kind not infered" ++ n

-- Todo:
renFExp :: A.Exp -> RN (Id, [Pat])

renFExp (A.VarExp n) = do
  qname_f <- qname (orig_name n)
  return (qname_f, [])

renFExp e@(A.FunAppExp _ _) = do
  renfexp' e []
  where
    renfexp' (A.FunAppExp e@(A.FunAppExp _ _) e') pats = do
      pat <- renPat e'
      renfexp' e (pat:pats)
    renfexp' (A.FunAppExp (A.VarExp n) e) pats = do
      qn <- qname (orig_name n)
      pat <- renPat e
      return (qn, pat:pats)

renFExp (A.InfixExp le op re) = do
  qname_op <- qname (orig_name op)
  lpat <- renPat le
  rpat <- renPat re
  return (qname_op, [lpat, rpat])

renFExp e = error $ "renFExp" ++ show e

renPat (A.VarExp n) | isConName n = do qn <- qname (orig_name n)
                                       x <- findCMs qn
                                       let a = case x of
                                             Just a' -> a'
                                             Nothing -> error $  "renPat error: " ++ qn
                                       return $ PCon a []
                    | otherwise   = do qn <- renameVar n
                                       return $ PVar qn

renPat (A.ParExp e) = renPat e

-- TODO: DRY! renExp and renPats have
renPat(A.InfixExp (A.InfixExp rest op2 e2) op1 e1) = do
  (prec1, fix1) <- lookupInfixOp op1
  (prec2, fix2) <- lookupInfixOp op2
  if prec1 == prec2 && (fix1 /= fix2 || fix1 == A.Infix)
    then fail "fixty resolution error."
    else if prec1 > prec2 || (prec1 == prec2 && fix1 == A.Infixr)
         then renPat (A.InfixExp rest op2 (opAppExp op1 e2 e1))
         else renPat (opAppExp op1 (A.InfixExp rest op2 e2) e1)
  where
    opAppExp op e e' = (A.FunAppExp (A.FunAppExp (A.VarExp op) e) e')

renPat (A.InfixExp e2 op e1) =
  renPat (A.FunAppExp (A.FunAppExp (A.VarExp op) e2) e1)

renPat (A.FunAppExp e e') = renPCon (A.FunAppExp e e') []
  where renPCon (A.FunAppExp (A.FunAppExp e e') e'') pats = do
          pat <- renPat e''
          renPCon (A.FunAppExp e e') (pat:pats)
        renPCon (A.FunAppExp (A.VarExp n) e') pats = do
          qn <- qname (orig_name n)
          Just a <- findCMs qn
          pat' <- renPat e'
          return $ PCon a (pat':pats)

renPat A.WildcardPat = return PWildcard

renPat e = error $ "renPat: " ++ show e

renRhs :: A.Rhs -> RN Expr
renRhs (A.UnguardedRhs (A.VarExp n) []) = do
  qname_c <- qname (orig_name n)
  c_pat   <- findCMs qname_c
  case c_pat of
    Just pat -> return (Const pat)
    Nothing | not (isConName n) -> return (Var qname_c)
            | otherwise -> trace "warning: unexpected pattern in renRhs." $
                           return (Var qname_c)


renRhs (A.UnguardedRhs e []) = renExp e

renRhs (A.UnguardedRhs e ds) =
  renRhs (A.UnguardedRhs (A.LetExp ds e) [])

renRhs rhs = do
  st <- get
  error $ "renRhs not yet implemented. " ++ show (st, rhs)

renExp (A.InfixExp (A.InfixExp rest op2 e2) op1 e1) = do
  (prec1, fix1) <- lookupInfixOp op1
  (prec2, fix2) <- lookupInfixOp op2
  if prec1 == prec2 && (fix1 /= fix2 || fix1 == A.Infix)
    then fail "fixty resolution error."
    else if prec1 > prec2 || (prec1 == prec2 && fix1 == A.Infixr)
         then renExp (A.InfixExp rest op2 (opAppExp op1 e2 e1))
         else renExp (opAppExp op1 (A.InfixExp rest op2 e2) e1)
  where
    opAppExp op e e' = (A.FunAppExp (A.FunAppExp (A.VarExp op) e) e')

renExp (A.InfixExp e2 op e1) = 
  renExp (A.FunAppExp (A.FunAppExp (A.VarExp op) e2) e1)

renExp (A.FunAppExp e1 e2) = do
  expr1 <- renExp e1
  expr2 <- renExp e2
  return (Ap expr1 expr2)

renExp (A.VarExp name) = do
  qn <- qname (orig_name name)
  return (Var qn)

renExp (A.LitExp (A.LitInteger i _)) = do
  return (Lit (LitInt i))

renExp (A.LetExp ds e) = do
  enterNewLevel
  (ds', _, _) <- collectNames ([], [], []) ds
  pushTbs
  tbs <- renDecls ds'
  popTbs
  e' <- renExp e
  exitLevel
  let bgs = toBg tbs
  return (Let (head bgs) e') -- TODO: (head bgs) is temporary

-- List comprehension
-- [e | True] = [e]
-- [e | q] = [q, True]
renExp (A.ListCompExp e [stmt@(A.ExpStmt p)]) =
  case p of
    A.VarExp n  | orig_name n == "True" -> renExp (A.ListExp [e])
                | otherwise             -> ren'
    _                                   -> ren'
  where ren' = renExp (A.ListCompExp e [stmt, A.ExpStmt aTrue])

-- [e | b, Q] = if b then [e | Q] else []
renExp (A.ListCompExp e ((A.ExpStmt b):stmts)) =
  renExp (A.IfExp b (A.ListCompExp e stmts) nil)
  where nil = A.VarExp $ Name "[]" "" (0,0) True

-- [e | p <- l, Q] = let ok p = [e | Q] in concatMap ok l
renExp (A.ListCompExp e ((A.BindStmt p l):stmts)) = renExp letexp
  where
    ok = Name "OK" "" (0,0) False -- "OK" is fresh,  will never parsed as a variable.
    okp = A.FunAppExp (A.VarExp ok) p
    rhs = case stmts of
      [] -> A.UnguardedRhs (A.ListExp [e]) []
      _  -> A.UnguardedRhs (A.ListCompExp e stmts) []
    decl = A.ValDecl okp rhs
    body = A.FunAppExp (A.FunAppExp (A.VarExp $ Name "concatMap" "" (0,0) False)
                                    (A.VarExp ok))
                       l
    letexp = A.LetExp [decl] body

-- [e | let decls, Q] = let decls in [e | Q]
renExp (A.ListCompExp e ((A.LetStmt decls):stmts)) = renExp letexp
  where
    body = case stmts of
      [] -> A.ListExp [e]
      _  -> A.ListCompExp e stmts
    letexp = A.LetExp decls body

renExp (A.IfExp c t f) = renExp caseexp
  where
    alt1 = A.Match aTrue (A.UnguardedRhs t [])
    alt2 = A.Match aFalse (A.UnguardedRhs f [])
    caseexp = A.CaseExp c [alt1, alt2]

renExp (A.CaseExp c alts) = renExp (A.LetExp decls fc)
  where f = Name "F" "" (0,0) False
        fc = A.FunAppExp (A.VarExp f) c
        decls = map (\(A.Match p rhs) ->
                      A.ValDecl (A.FunAppExp (A.VarExp f) p) rhs) alts

renExp (A.ListExp [e]) =
  renExp $ A.FunAppExp (A.FunAppExp aCons e) aNil

renExp (A.ListExp (e:es)) =
  renExp $ A.FunAppExp (A.FunAppExp aCons e) (A.ListExp es)

renExp (A.DoExp [stmt]) = case stmt of
  A.ExpStmt e -> renExp e
  _ -> fail "The last statement in a 'do' block must be an expression"

renExp (A.DoExp (A.ExpStmt e:stmts)) =
  renExp $ A.FunAppExp (A.FunAppExp aThen e) (A.DoExp stmts)

renExp (A.DoExp ((A.BindStmt p e):stmts)) = renExp letexp
  where
    ok = Name "OK" "" (0,0) False
    okp = A.FunAppExp (A.VarExp ok) p
    rhs = A.UnguardedRhs (A.DoExp stmts) []
    decl = A.ValDecl okp rhs
    body = A.FunAppExp (A.FunAppExp aBind e) (A.VarExp ok)
    letexp = A.LetExp [decl] body

renExp (A.DoExp ((A.LetStmt decls):stmts)) = renExp letexp
  where
    letexp = A.LetExp decls (A.DoExp stmts)

renExp (A.LamExp args e) = renExp (A.LetExp [decl] f)
  where f = A.VarExp $ Name "F" "" (0,0) False
        fexp = foldl' A.FunAppExp f args
        rhs = A.UnguardedRhs e []
        decl = A.ValDecl fexp rhs

renExp (A.LitExp (A.LitString s _)) =
  return $ Lit (LitStr s)

renExp (A.LitExp (A.LitInteger i _)) =
  return $ Lit (LitInt i)

renExp (A.LitExp (A.LitChar c _)) =
  return $ Lit (LitChar c)

renExp (A.ParExp e) = renExp e

renExp e = error $ "Non-exhaustive patterns in renExp: " ++ show e

lookupInfixOp :: Name -> RN (Int, A.Fixity)
lookupInfixOp op = do
  op_qname <- qname (orig_name op)
  st <- get
  case tabLookup op_qname (rn_ifxenv st) of
    Just (LeftAssoc  x) -> return (x, A.Infixl)
    Just (RightAssoc x) -> return (x, A.Infixr)
    Just (NoAssoc    x) -> return (x, A.Infix)
    Nothing             -> return (9, A.Infixl)

qname   :: Id -> RN Id
qname name = state $ \st@RnState{rn_lvs=lvs} -> (findQName lvs name, st)
  where findQName [] n = error $ "qname not found: " ++ n
        findQName (lv:lvs) n = case tabLookup n (lv_dict lv) of
          Just qn -> qn
          Nothing -> findQName lvs n

findCMs   :: Id -> RN (Maybe Assump)
findCMs qn = state $ \st@RnState{rn_cms=as} -> (find' as qn, st)
  where find' [] _ = Nothing
        find' (a@(n :>: _):as') qn' | n == qn'  = Just a
                                    | otherwise = find' as' qn'

pushLv :: Level -> RN ()
pushLv lv = state $ \st@RnState{rn_lvs=lvs} ->
  ((), st{rn_lvs=(lv:lvs)})

getPrefix :: RN Id
getPrefix = state $ \st@RnState{rn_lvs=(lv:_)} -> (lv_prefix lv, st)

newNum :: RN Int
newNum = state $ \st@RnState{rn_lvs=(lv:lvs)} ->
  let n   = lv_num lv
      lv' = lv{lv_num=n+1}
  in (n, st{rn_lvs=(lv':lvs)})

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
  let lvs' = tail $ rn_lvs st
  put st{rn_lvs=lvs'}

{-
toBg :: [TempBind] -> [BindGroup]
toBg tbs = toBg2 tbs' scdict
  where (tbs', scdict) = toBg1 tbs [] empty
        toBg1 :: [TempBind] -> [TempBind] -> Table Scheme -> ([TempBind], Table Scheme)
        toBg1 [] tbs2 dct = (reverse tbs2, dct)
        toBg1 (tb:tbs1) tbs2 dct = case tb of
          (name, Just qt, alts)  -> let ts = tv qt
                                        scm' = quantify ts qt
                                        dct' = insert name scm' dct
                                        tbs2' = tbAdd tbs2 name alts
                                    in toBg1 tbs1 tbs2' dct'
          (name, Nothing, alts)  -> let tbs2' = tbAdd tbs2 name alts
                                    in toBg1 tbs1 tbs2' dct
        tbAdd [] name alts = [(name, Nothing, alts)]
        tbAdd tbs2 name alts =
          let s2lv = length.(filter (== '.'))
              flt (n, _, _) = s2lv n > s2lv name
              (ts1, ts2) = span flt tbs2
          in case ts2 of
            ((n, _, a):ts) | n == name -> ts1 ++ (n, Nothing, a ++ alts) : ts
                           | otherwise -> (name, Nothing, alts) : tbs2
            _ -> (name, Nothing, alts) : tbs2
        toBg2 :: [TempBind] -> Table Scheme -> [BindGroup]
        toBg2 tbs2 dct = tobg2 [([], [[]])] tbs2
          where tobg2 bg [] = bg
                tobg2 [(es, [is])] (tb:tbs) = case tb of
                  (_, _, []) -> tobg2 [(es, [is])] tbs
                  (n, _, as) -> case tabLookup n dct of
                    Just scm -> tobg2 [((n, scm, as):es, [is])] tbs
                    Nothing  -> tobg2 [(es, [(n, as):is])] tbs
-}

toBg :: [TempBind] -> [BindGroup]
toBg tbs = [toBg2 tbs]

toBg2 :: [TempBind] -> BindGroup
toBg2 tbs =
  let
    (h, rh, idx) = vars tbs
    deps = map tbDepend tbs

    -- Calculating SCC
    edges = concat $ map (d2es h) deps
    g' = G.buildG (0, idx - 1) edges
    sccs = map T.flatten $ G.scc g'

    -- Preparation for TempBind->BindGroup translation
    scdict = collectTypes tbs
    bm = bindMap tbs h
  in
   scc2bg sccs bm scdict

vars :: [TempBind] -> (Map.Map Id Int, Map.Map Int Id, Int)
vars tbs = vars' tbs (Map.empty, Map.empty) 0
  where
    vars' [] (h, rh) i = (h, rh, i)
    vars' ((_,_,[]):tbs) (h, rh) i = vars' tbs (h, rh) i
    vars' ((n,_,_):tbs) (h, rh) i =
      case Map.lookup n h of
        Just _ -> vars' tbs (h, rh) i
        Nothing -> vars' tbs (Map.insert n i h, Map.insert i n rh) (i+1)

boundvars :: [Pat] -> [Id]
boundvars ps = concat $ map boundvar ps

boundvar :: Pat -> [Id]
boundvar (PVar n) = [n]
boundvar PWildcard = []
boundvar (PAs n p) = n : boundvar p
boundvar (PLit _) = []
boundvar (PCon _ ps) = concat $ map boundvar ps

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
    altsfv alts = concat $ map fv alts
    vs1 = concat $ map (\(_, _, alts) -> altsfv alts) es
    vs2 = concat $ map (\(_, alts) -> altsfv alts) (concat iss)
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
tbDepend (n, _, alts) = (n, concat $ map fv alts)

d2es :: Map.Map Id Int -> (Id, [Id]) -> [G.Edge]
d2es h (n, vs) =
  let
    src = case Map.lookup n h of { Just i -> i }

    f v = case Map.lookup v h of
      Just i -> [i]
      Nothing -> []

    dests = concat $ map f vs
  in
   zip (repeat src) dests
                           
collectTypes :: [TempBind] -> Map.Map Id Scheme
collectTypes tbs = collty tbs (Map.empty)
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
bindMap tbs rh = bmap tbs rh (Map.empty)
  where
    bmap [] _ d = d
    bmap ((_, _, []):tbs) h d = bmap tbs h d
    bmap (tb@(name, _, alts):tbs) h d =
      let
        i = case Map.lookup name h of
          Just i' -> i'
          Nothing -> error $ "Must not happen. Vertex Id not found: " ++ name

        tb' = case Map.lookup i d of
          Just (_, _, alts') -> (name, Nothing, alts++alts')
          Nothing -> tb

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
          _ -> (is':iss)
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
          Nothing -> (es, (name, alts):is)
      in
       cnvScc xs es' is' 

        


