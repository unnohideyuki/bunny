module Semant (
    FixtyInfo(..)
  , DictDef(..)
  , RnState(..)
  , Level(..)
  , initialLevel
  , renProg
  , renPrelude
  ) where

import           Control.Monad              (when)
import           Control.Monad.State.Strict (State, get, put, state)
import qualified Data.Graph                 as G
import           Data.List                  (concatMap, foldl', notElem, (\\))
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import qualified Data.Tree                  as T

import qualified Absyn                      as A
import           PreDefined
import           Symbol
import           Types
import           Typing

econst  :: Assump -> Expr
econst = Const

aTrue :: A.Exp
aTrue  = A.VarExp $ Name "True" (0,0) True

aFalse :: A.Exp
aFalse  = A.VarExp $ Name "False" (0,0) True

aThen :: A.Exp
aThen  = A.VarExp $ Name ">>" (0,0) False

nNil :: Name
nNil = Name "[]" (0,0) True

aNil :: A.Exp
aNil  = A.VarExp nNil

aCons :: A.Exp
aCons  = A.VarExp $ Name ":" (0,0) True

aBind :: A.Exp
aBind  = A.VarExp $ Name ">>=" (0,0) False

data FixtyInfo = LeftAssoc  Int
               | RightAssoc Int
               | NoAssoc    Int
                 deriving (Show, Eq)

data Level = Level { lvPrefix :: !Id
                   , lvDict   :: !(Table Id)
                   , lvNum    :: !Int
                   }
             deriving Show

initialLevel      :: Maybe Name -> Level
initialLevel modid = Level { lvPrefix = case modid of
                                Just s  -> origName s
                                Nothing -> "Main"
                           , lvDict   = empty
                           , lvNum    = 0
                           }

-- Renaming Monad

data RnState = RnState { rnstatModid    :: !Id
                       , rnstatLvs      :: ![Level]
                       , rnstatTenv     :: !(Table Id)
                       , rnstatIfxenv   :: !(Table FixtyInfo)
                       , rnstatCe       :: !ClassEnv
                       , rnstatCms      :: ![Assump]
                       , rnstatTbs      :: ![TempBind]
                       , rnstatTbsStack :: ![[TempBind]]
                       , rnstatKdict    :: !(Table Kind)
                       , rnstatCdicts   :: ![DictDef]
                       }
               deriving Show

type RN a = State RnState a

putCDicts :: [DictDef] -> RN()
putCDicts dicts= do
  st <- get
  put st{rnstatCdicts = dicts}

getCDicts :: RN [DictDef]
getCDicts = do
  st <- get
  return $ rnstatCdicts st


lookupCDicts :: Id -> RN DictDef
lookupCDicts qcname = do
  dicts <- getCDicts
  let dicts' = dropWhile (\d -> dictdefQclsname d /= qcname) dicts
  case dicts' of
    [] -> error $ "lookupCDicts: class not found: " ++ show (qcname, dicts')
    _  -> return $ head dicts'

pushTbs :: RN ()
pushTbs = do
  st <- get
  let tbs = rnstatTbs st
      tbstack = rnstatTbsStack st
  put st{rnstatTbs=[], rnstatTbsStack= tbs:tbstack}

popTbs :: RN ()
popTbs = do
  st <- get
  let (tbs', tbstack') = case rnstatTbsStack st of
        (x:xs) -> (x, xs)
        []     -> error "popTbs from empty stack."
  put st{rnstatTbs=tbs', rnstatTbsStack=tbstack'}

renameVar :: Name -> RN Id
renameVar name = state $ \st@RnState{rnstatLvs=(lv:lvs)} ->
  let
    prefix = lvPrefix lv
    n  = origName name
    n' = prefix ++ "." ++ n
    dict' = insert n n' (lvDict lv)
    lv' = lv{lvDict=dict'}
  in
   (n', st{rnstatLvs= lv':lvs})

regFixity :: A.Fixity -> Int -> [Name] -> RN ()
regFixity _ _ [] = return ()
regFixity fixity i (n:ns) = do reg (f i) n; regFixity fixity i ns
  where f = case fixity of
          A.Infixl -> LeftAssoc
          A.Infixr -> RightAssoc
          A.Infix  -> NoAssoc
        reg finfo name = state $ \st@RnState{rnstatLvs=(lv:_), rnstatIfxenv=ifxenv} ->
          let
            qn = lvPrefix lv ++ "." ++ origName name
            ifxenv' = insert qn finfo ifxenv
          in
           if defined (tabLookup qn ifxenv) then
             error $ "duplicate fixity declaration:" ++ qn
           else
             ((), st{rnstatIfxenv=ifxenv'})

extrName :: A.Exp -> Name
extrName (A.VarExp name)       = name
extrName (A.FunAppExp f _)     = extrName f
extrName (A.InfixExp _ name _) = name
extrName e                     = error $ "unexpected exp:" ++ show e

collectNames :: ([A.Decl], [A.Decl], [A.Decl]) -> [A.Decl]
                -> RN ([A.Decl], [A.Decl], [A.Decl])
collectNames x [] = return x
collectNames (ds, cds, ids) (dcl:dcls) = do
  (ds', cds', ids') <- collname dcl
  collectNames (ds', cds', ids') dcls
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


    collname d@A.InstDecl{} = return (ds, cds, ids ++ [d])

    collname A.DataDecl{}         = error "not yet: DataDecl"
    collname A.NewtypeDecl{}      = error "not yet: NewtypeDecl"

    collname _ = error "Sement.collectNames.collname"

type TempBind = (Id, Maybe (Qual Type), [Alt])

data DictDef = DictDef{ dictdefQclsname :: Id
                      , dictdefMethods  :: [Id]
                      , dictdefDecls    :: [A.Decl]
                      }
              deriving Show

cdecl2dict :: Id -> A.Decl -> DictDef
cdecl2dict modid (A.ClassDecl (_, A.AppTy (A.Tycon n) _) ds) =
  let
    name = modid ++ "." ++ origName n

    extrMName (A.TypeSigDecl ns _) = map origName ns
    extrMName _                    = []

    ms = concatMap extrMName ds

    extrValDecl d@(A.ValDecl _ _) = [d]
    extrValDecl _                 = []

    vdcls = concatMap extrValDecl ds
  in
   DictDef{dictdefQclsname = name, dictdefMethods = ms, dictdefDecls = vdcls}

cdecl2dict _ _ = error "Semant.cdecl2dict"

renProgCommon ::
  A.Module
  -> RN ([BindGroup], [BindGroup], [Assump], [DictDef] ,[(Id, Id)])
renProgCommon m = do
  let body = snd (A.body m)
      modid = case A.modid m of
        Just Name{origName=s} -> s
        Nothing               -> "Main"
  (ds, cds, ids) <- collectNames ([], [], []) body
  ctbs <- renCDictdefDecls cds []
  let bgs' = toBg ctbs
      as2 = map (\(n, scm, _) -> n :>: scm) $ fst $ head bgs'
  let dicts = map (cdecl2dict modid) cds
  putCDicts dicts
  (itbs, ctab) <- renIDictdefDecls ids
  tbs <- renDictdefDecls ds
  -- NOTE#1: followings are not clear! see the note page 233.
  let bgs = toBg $ tbs ++ itbs
      bgs'' = toBg $ ctbs ++ tbs ++ itbs
  return (bgs, bgs'', as2, dicts, ctab)

renProg :: A.Module -> (Subst, Int, [Assump])
           -> RN ([BindGroup], [Assump], [DictDef] ,[(Id, Id)])
renProg m cont = do
  (bgs, bgs'', as2, dicts, ctab) <- renProgCommon m
  st <- get
  let ce = rnstatCe st
      as = rnstatCms st
      as' = tiProgram ce (as ++ as2) bgs cont
  return (bgs'', as' ++ as2, dicts, ctab)

renPrelude :: A.Module -> RN (Subst, Int, [Assump])
renPrelude m = do
  (bgs, _, as2, _, _) <- renProgCommon m
  st <- get
  let ce = rnstatCe st
      as = rnstatCms st
  return $ tiImportedProgram ce (as ++ as2) bgs initialTI

renCDictdefDecls :: [A.Decl] -> [TempBind] -> RN [TempBind]
renCDictdefDecls [] tbs = return tbs
renCDictdefDecls (A.ClassDecl cls ds : cds) tbs = do
  cname <- clsadd cls
  let ds' = suppDs ds cname
  tbs' <- renDictdefDecls $ addvar cls ds'
  renCDictdefDecls cds (tbs ++ tbs')
  where clsadd (_, A.AppTy (A.Tycon n) _) = do
          cname <- qname $ origName n
          st <- get
          let ce = rnstatCe st
              ce' = -- todo: super class
                fromMaybe (error $ "addClass failed: " ++ show (cname, ce))
                  (addClass cname [] ce)
          put $ st{rnstatCe=ce'}
          return cname
        clsadd _ = error "Semant.renCDictdefDecls.clsadd"
        addvar c ds' = let (_, sigvar) = c
                           f (A.TypeSigDecl ns (_, sigdoc)) =
                             A.TypeSigDecl ns (Just sigvar, sigdoc)
                           f d = d
                        in map f ds'

renCDictdefDecls _ _ = error "Sement.renCDictdefDecls"

{- suppDs -- Exstract TypeSigDictdefDecls and supplement ValDictdefDecls that defines
             overloaded functions.
-}
suppDs :: [A.Decl] -> String -> [A.Decl]
suppDs ds clsname =
  let
    ubNames [] cns cds' = (cns, cds')

    ubNames (cd@(A.TypeSigDecl ns _):ds'') cns cds' =
       ubNames ds'' (cns ++ map origName ns) (cd : cds')

    ubNames (A.ValDecl _ _ : ds'') cns cds' = ubNames ds'' cns cds'

    ubNames _ _ _ = error "Sement.suppDs.ubNames"

    (ns', cds) = ubNames ds [] []

    mkv n = A.VarExp Name{origName=n, namePos=(-1, -1), isConName=False}
    mkoldcl n = A.ValDecl (mkv n) (A.UnguardedRhs
                                   (A.FunAppExp
                                    (A.FunAppExp (mkv "#overloaded#") (mkv n))
                                    (A.LitExp (A.LitString clsname undefined))
                                   )
                                   [])

    ds' = map mkoldcl ns'
  in
   cds ++ ds'

renIDictdefDecls :: [A.Decl] -> RN ([TempBind], [(Id, Id)])
renIDictdefDecls [] = return ([], [])
renIDictdefDecls (A.InstDecl ctx t ds : ids) = do
  (qcn, qin, i, a) <- case t of
    (A.AppTy (A.Tycon n1) (A.Tycon n2)) -> do qcn <- qname $ origName n1
                                              qin <- renameVar n2
                                              return (qcn, qin, n2, "")
    (A.AppTy (A.Tycon n1) (A.ListTy (A.Tyvar n2))) -> do
      qcn <- qname $ origName n1
      qin <- renameVar nNil
      return (qcn, qin, nNil, origName n2)

    _ -> error $ "Non-exhaustive pattern in case: " ++ show t

  k <- lookupKdict qcn
  let p = case origName i of
        "[]" -> IsIn qcn (TAp (TCon (Tycon qin k)) (TVar (Tyvar a Star)))
        _    -> IsIn qcn (TCon (Tycon qin k))

  ps <- tops ctx
  instAdd ps p

  dict <- lookupCDicts qcn
  let defds = dictdefDecls dict
      ds' = mergeDs ds defds
  enterNewLevelWith $ "I%" ++ origName i -- see STG/isLocal
  (ds'', _, _) <- collectNames ([], [], []) ds'
  tbs <- renDictdefDecls ds''
  exitLevel

  (tbs', ctab') <- renIDictdefDecls ids
  return (tbs ++ tbs', (qin, qcn):ctab')
  where instAdd ps p = do
          st <- get
          let ce = rnstatCe st
              ce' = fromMaybe (error $ "addInst failed: " ++ show (p, ce))
                      (addInst ps p ce)
          put $ st{rnstatCe=ce'}

        extrId' (A.ValDecl e _) = origName $ extrName e
        extrId' _               = error "extrId': unexpected"

        mergeDs dcls defdictdefDecls =
          let
            names = map extrId' dcls
            ds2 = filter (\d -> extrId' d `notElem` names) defdictdefDecls
          in
           dcls ++ ds2

        tops Nothing  = return []
        tops (Just x) = tops' x

        tops' (A.ParTy x) = tops' x
        tops' (A.AppTy (A.Tycon n1) (A.Tyvar n2)) = do
          qcn <- qname $ origName n1
          let x = TVar $ Tyvar (origName n2) Star -- TODO: always works?
          return [IsIn qcn x]
        tops' _ = error "tops': unexpected"


renIDictdefDecls _ = error "renIdictdefDecls: must not occur"

lookupKdict :: Id -> RN Kind
lookupKdict n = do
  st <- get
  case tabLookup n (rnstatKdict st) of
    Just k  -> return k
    Nothing -> error $ "kind not found: " ++ n

renDictdefDecls :: [A.Decl] -> RN [TempBind]
renDictdefDecls [] = do
  st <- get
  put st{rnstatTbs=[]}
  return $ rnstatTbs st
renDictdefDecls (d:ds) = do
  ntbs <- renDecl d
  st <- get
  let tbs = rnstatTbs st
      tbs' = tbs++ntbs
  put st{rnstatTbs=tbs'}
  renDictdefDecls ds

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

renDecl _ = return [("", Nothing, [])]

kiExpr :: A.Type -> [(Id, Kind)] -> [(Id, Kind)]
kiExpr (A.FunTy t1 t2) dict =
  let dict' = kiExpr t1 dict
  in kiExpr t2 dict'
kiExpr (A.AppTy t1 t2) dict = dict ++ [(extrid t1, Kfun Star Star)
                                      ,(extrid t2, Star)]
  where extrid (A.Tyvar n) = origName n
        extrid _           = "extrid: unexpected"
kiExpr (A.Tyvar n) dict = dict ++ [(origName n, Star)]
kiExpr (A.ParTy e) dict = kiExpr e dict
kiExpr (A.Tycon _) dict = dict
kiExpr (A.ListTy e) dict = kiExpr e dict
kiExpr t dict = error $ "kiExpr: " ++ show (t, dict)

insertKdict :: Id -> Kind -> RN ()
insertKdict n k = do
  st <- get
  let kdict = rnstatKdict st
      kdict' = insert n k kdict
  put st{rnstatKdict=kdict'}

renSigvar :: Maybe A.Type -> [(Id, Kind)] -> RN [Pred]
renSigvar Nothing _ = return []
renSigvar (Just (A.AppTy (A.Tycon n) (A.Tyvar m))) kdict = do
  qn <- qname $ origName n
  let vname = origName m
      k = kindLookup vname kdict
  when (isConName n) (insertKdict qn k)
  return [IsIn qn (TVar (Tyvar vname k))]

renSigvar _ _ = error "renSigvar"

renSigdoc :: A.Type -> [(Id, Kind)] -> RN Type
renSigdoc (A.FunTy e1 e2) kdict = do
  t1 <- renSigdoc e1 kdict
  t2 <- renSigdoc e2 kdict
  return (t1 `fn` t2)
renSigdoc (A.Tyvar n) kdict = let vname = origName n
                                  k = kindLookup vname kdict
                              in return (TVar (Tyvar vname k))
renSigdoc (A.AppTy e1 e2) kdict = do
  t1 <- renSigdoc e1 kdict
  t2 <- renSigdoc e2 kdict
  return (TAp t1 t2)

renSigdoc (A.ParTy e) kdict = renSigdoc e kdict

-- TODO: should be fix this hard coding.
renSigdoc (A.Tycon n) _ = case origName n of
  "Integer" -> return tInteger
  "Int"     -> return tInt
  "String"  -> return tString
  "IO"      -> return $ TCon (Tycon "IO" (Kfun Star Star))
  "()"      -> return tUnit
  "Bool"    -> return tBool
  s         -> error $ "renSigDoc $ A.Tycon " ++ s

renSigdoc (A.ListTy e) kdict = do
  t <- renSigdoc e kdict
  return $ list t

renSigdoc t _ = error $ "renSigdoc" ++ show t

kindLookup :: Id -> [(Id, Kind)] -> Kind
kindLookup n kdict =
  fromMaybe (error $ "Kind not infered" ++ n) (lookup n kdict)

-- Todo:
renFExp :: A.Exp -> RN (Id, [Pat])

renFExp (A.VarExp n) = do
  qname_f <- qname (origName n)
  return (qname_f, [])

renFExp f@(A.FunAppExp _ _) = renfexp' f []
  where
    renfexp' (A.FunAppExp e@(A.FunAppExp _ _) e') pats = do
      pat <- renPat e'
      renfexp' e (pat:pats)
    renfexp' (A.FunAppExp (A.VarExp n) e) pats = do
      qn <- qname (origName n)
      pat <- renPat e
      return (qn, pat:pats)
    renfexp' _ _ = error "renfexp': unexpected"

renFExp (A.InfixExp le op re) = do
  qname_op <- qname (origName op)
  lpat <- renPat le
  rpat <- renPat re
  return (qname_op, [lpat, rpat])

renFExp e = error $ "renFExp" ++ show e

renPat :: A.Exp -> RN Pat
renPat (A.VarExp n) | isConName n = do qn <- qname (origName n)
                                       x <- findCMs qn
                                       let a = fromMaybe
                                               (error $  "renPat error: " ++ qn)
                                               x
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
    opAppExp op e = A.FunAppExp (A.FunAppExp (A.VarExp op) e)

renPat (A.InfixExp e2 op e1) =
  renPat (A.FunAppExp (A.FunAppExp (A.VarExp op) e2) e1)

renPat (A.FunAppExp f f') = renPCon (A.FunAppExp f f') []
  where renPCon (A.FunAppExp (A.FunAppExp e e') e'') pats = do
          pat <- renPat e''
          renPCon (A.FunAppExp e e') (pat:pats)
        renPCon (A.FunAppExp (A.VarExp n) e') pats = do
          qn <- qname (origName n)
          Just a <- findCMs qn
          pat' <- renPat e'
          return $ PCon a (pat':pats)
        renPCon _ _ = error "renPCon: unexpected"

renPat (A.TupleExp [Just e1, Just e2]) = do
  p1 <- renPat e1
  p2 <- renPat e2
  return $ PCon pairCfun [p1, p2]

renPat A.WildcardPat = return PWildcard

renPat e = error $ "renPat: " ++ show e

renRhs :: A.Rhs -> RN Expr
renRhs (A.UnguardedRhs (A.VarExp n) []) = do
  qname_c <- qname (origName n)
  c_pat   <- findCMs qname_c
  case c_pat of
    Just pat -> return (Const pat)
    Nothing | not (isConName n) -> return (Var qname_c)
            | otherwise -> return (Var qname_c)


renRhs (A.UnguardedRhs e []) = renExp e

renRhs (A.UnguardedRhs e ds) =
  renRhs (A.UnguardedRhs (A.LetExp ds e) [])

renRhs rhs = do
  st <- get
  error $ "renRhs not yet implemented. " ++ show (st, rhs)

renExp :: A.Exp -> RN Expr

renExp (A.InfixExp (A.InfixExp rest op2 e2) op1 e1) = do
  (prec1, fix1) <- lookupInfixOp op1
  (prec2, fix2) <- lookupInfixOp op2
  if prec1 == prec2 && (fix1 /= fix2 || fix1 == A.Infix)
    then fail "fixty resolution error."
    else if prec1 > prec2 || (prec1 == prec2 && fix1 == A.Infixr)
         then renExp (A.InfixExp rest op2 (opAppExp op1 e2 e1))
         else renExp (opAppExp op1 (A.InfixExp rest op2 e2) e1)
  where
    opAppExp op e = A.FunAppExp (A.FunAppExp (A.VarExp op) e)

renExp (A.InfixExp e2 op e1) =
  renExp (A.FunAppExp (A.FunAppExp (A.VarExp op) e2) e1)

renExp (A.FunAppExp e1 e2) = do
  expr1 <- renExp e1
  expr2 <- renExp e2
  return (Ap expr1 expr2)

renExp (A.VarExp name) = do
  qn <- qname (origName name)
  return (Var qn)

renExp (A.LetExp ds e) = do
  enterNewLevel
  (ds', _, _) <- collectNames ([], [], []) ds
  pushTbs
  tbs <- renDictdefDecls ds'
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
    A.VarExp n  | origName n == "True" -> renExp (A.ListExp [e])
                | otherwise             -> ren'
    _                                   -> ren'
  where ren' = renExp (A.ListCompExp e [stmt, A.ExpStmt aTrue])

-- [e | b, Q] = if b then [e | Q] else []
renExp (A.ListCompExp e (A.ExpStmt b : stmts)) =
  renExp (A.IfExp b (A.ListCompExp e stmts) nil)
  where nil = A.VarExp $ Name "[]" (0,0) True

-- [e | p <- l, Q] = let ok p = [e | Q] in concatMap ok l
renExp (A.ListCompExp e (A.BindStmt p l : stmts)) = renExp letexp
  where
    ok = Name "OK" (0,0) False -- "OK" is fresh,  will never parsed as a variable.
    okp = A.FunAppExp (A.VarExp ok) p
    rhs = case stmts of
      [] -> A.UnguardedRhs (A.ListExp [e]) []
      _  -> A.UnguardedRhs (A.ListCompExp e stmts) []
    decl = A.ValDecl okp rhs
    body = A.FunAppExp (A.FunAppExp (A.VarExp $ Name "concatMap" (0,0) False)
                                    (A.VarExp ok))
                       l
    letexp = A.LetExp [decl] body

-- [e | let dictdefDecls, Q] = let dictdefDecls in [e | Q]
renExp (A.ListCompExp e (A.LetStmt ddecls : stmts)) = renExp letexp
  where
    body = case stmts of
      [] -> A.ListExp [e]
      _  -> A.ListCompExp e stmts
    letexp = A.LetExp ddecls body

renExp (A.IfExp c t f) = renExp caseexp
  where
    alt1 = A.Match aTrue (A.UnguardedRhs t [])
    alt2 = A.Match aFalse (A.UnguardedRhs f [])
    caseexp = A.CaseExp c [alt1, alt2]

renExp (A.CaseExp c alts) = renExp (A.LetExp ddecls fc)
  where f = Name "F" (0,0) False
        fc = A.FunAppExp (A.VarExp f) c
        ddecls = map (\(A.Match p rhs) ->
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

renExp (A.DoExp (A.BindStmt p e : stmts)) = renExp letexp
  where
    ok = Name "OK" (0,0) False
    okp = A.FunAppExp (A.VarExp ok) p
    rhs = A.UnguardedRhs (A.DoExp stmts) []
    decl = A.ValDecl okp rhs
    body = A.FunAppExp (A.FunAppExp aBind e) (A.VarExp ok)
    letexp = A.LetExp [decl] body

renExp (A.DoExp (A.LetStmt ddecls : stmts)) = renExp letexp
  where
    letexp = A.LetExp ddecls (A.DoExp stmts)

renExp (A.LamExp args e) = renExp (A.LetExp [decl] f)
  where f = A.VarExp $ Name "F" (0,0) False
        fexp = foldl' A.FunAppExp f args
        rhs = A.UnguardedRhs e []
        decl = A.ValDecl fexp rhs

renExp (A.LitExp (A.LitString s _)) = return $ Lit (LitStr s)

renExp (A.LitExp (A.LitInteger i _)) = return $ Lit (LitInt i)

renExp (A.LitExp (A.LitChar c _)) = return $ Lit (LitChar c)

renExp (A.ParExp e) = renExp e

-- pair
renExp (A.TupleExp [Just a, Just b]) = do
  e1 <- renExp a
  e2 <- renExp b
  let c = econst pairCfun
  return $ Ap (Ap c e1) e2

renExp e = error $ "Non-exhaustive patterns in renExp: " ++ show e

lookupInfixOp :: Name -> RN (Int, A.Fixity)
lookupInfixOp op = do
  op_qname <- qname (origName op)
  st <- get
  case tabLookup op_qname (rnstatIfxenv st) of
    Just (LeftAssoc  x) -> return (x, A.Infixl)
    Just (RightAssoc x) -> return (x, A.Infixr)
    Just (NoAssoc    x) -> return (x, A.Infix)
    Nothing             -> return (9, A.Infixl)

qname   :: Id -> RN Id
qname name = state $ \st@RnState{rnstatLvs=lvs} ->
  let {qn = findQName lvs name} in {-trace ("qn:" ++ show(name, qn))-} (qn, st)
  where findQName [] n = error $ "qname not found: " ++ n
        findQName (lv:lvs) n =
          fromMaybe (findQName lvs n) (tabLookup n (lvDict lv))

findCMs   :: Id -> RN (Maybe Assump)
findCMs qn = state $ \st@RnState{rnstatCms=as} -> (find' as qn, st)
  where find' [] _ = Nothing
        find' (a@(n :>: _):as') qn' | n == qn'  = Just a
                                    | otherwise = find' as' qn'

pushLv :: Level -> RN ()
pushLv lv = state $ \st@RnState{rnstatLvs=lvs} ->
  ((), st{rnstatLvs = lv:lvs})

getPrefix :: RN Id
getPrefix = state $ \st@RnState{rnstatLvs=(lv:_)} -> (lvPrefix lv, st)

newNum :: RN Int
newNum = state $ \st@RnState{rnstatLvs=(lv:lvs)} ->
  let n   = lvNum lv
      lv' = lv{lvNum=n+1}
  in (n, st{rnstatLvs = lv':lvs})

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
  let lvs' = tail $ rnstatLvs st
  put st{rnstatLvs=lvs'}

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
