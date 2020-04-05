module Rename ( module RenUtil
              , scanDecls
              , trCdecl
              , renClassDecls
              , renInstDecls
              , renDecls
              )where

import qualified Absyn                      as A
import           BindGrouping
import           PPTypes
import           PreDefined
import           RenUtil
import           Symbol
import           Types
import           Typing

import           Control.Monad              (when)
import           Control.Monad.State.Strict (get, put)
import           Data.List                  (concatMap, foldl', notElem)
import           Data.Maybe                 (fromMaybe)
import           Debug.Trace

scanDecls :: [A.Decl] -> RN ([A.ValueDecl], [A.ClassDecl], [A.InstDecl])
scanDecls ds = do
  r <- mapM scandecl ds
  let (dss, cdss, idss) = unzip3 r
  return (concat dss, concat cdss, concat idss)
  where
    scandecl (A.VDecl d@(A.ValDecl e _)) = do _ <- renameVar (extrName e)
                                              return ([d], [], [])

    scandecl (A.VDecl d@(A.TypeSigDecl _ _)) = return ([d], [], [])

    scandecl (A.CDecl d@(A.ClassDecl (_, A.AppTy (A.Tycon n) _) ds')) = do
      _ <- renameVar n
      mapM_ colname' ds'
      return ([], [d], [])
      where colname' (A.VDecl (A.ValDecl e _)) = do _ <- renameVar (extrName e)
                                                    return ()
            colname' _ = return ()

    {- TODO: consider followin patterns.
            (A.CDecl (A.ClassDecl (_, A.Tyvar _) _))
            (A.CDecl (A.ClassDecl (_, A.Tycon _) _))
            (A.CDecl (A.ClassDecl (_, A.FunTy _ _) _))
            (A.CDecl (A.ClassDecl (_, A.AppTy (A.Tyvar _) _) _))
    -}
    scandecl (A.CDecl (A.ClassDecl (_, _) _)) = error "scandecl: unexpected."

    scandecl (A.IDecl d) = return ([], [], [d])

    scandecl (A.FixSigDecl fixity i ns) = do regFixity fixity i ns
                                             return ([], [], [])

    scandecl d@(A.DataDecl (maybe_context, ty) consts maybe_dtys) = do
      let (tn, tvs) = parseTy ty
      qtn <- renameVar tn
      let k = foldr Kfun Star (replicate (length tvs) Star)
          t = foldl TAp (TCon (Tycon qtn k)) tvs

      let cs = map (\(A.Con t) -> parseTy t) consts
          renCs (n, ts) = do
            qn <- renameVar n
            let t' = foldr fn t ts
            return $ qn :>: toScheme t'

      as <- mapM renCs cs
      appendCMs as

      da <- mapM parseConsts cs
      let dc = map (\(n, _) -> (n, as)) da
      appendConstInfo da dc
      return ([], [], [])
      where
        parseTy (A.Tycon n) = (n, [])
        parseTy t = parsety' [] t
          where
            parsety' tvs (A.AppTy (A.Tycon cn) t2) = (cn, renTy t2 : tvs)
            parsety' tvs (A.AppTy t t2) = parsety' (renTy t2 : tvs) t

        renTy (A.Tyvar i) = TVar (Tyvar (origName i) Star)

        renTy (A.Tycon i) = case origName i of  -- TODO:
          "Int"  -> tInt
          "Integer" -> tInteger
          "Char" -> tChar
          x -> error $ "Non-exhaustive patterns: " ++ x

        renTy (A.ListTy t) = list (renTy t)

        parseConsts (n, ts) = do
          qn <- renameVar n
          let aty = length ts
          return (qn, aty)

    scandecl (A.DefaultDecl _)   = error "not yet: DefaultDecl"
    scandecl (A.ForeignDecl _)   = error "not yet: ForeignDecl"
    scandecl (A.SynonymDecl _ _) = error "not yet: SynonymDecl"
    scandecl A.NewtypeDecl{}     = error "not yet: NewtypeDecl"

trCdecl :: Id -> A.ClassDecl -> DictDef
trCdecl modid (A.ClassDecl (_, A.AppTy (A.Tycon n) _) ds) =
  let
    name = modid ++ "." ++ origName n

    extrMName (A.VDecl (A.TypeSigDecl ns _)) = map origName ns
    extrMName _                              = [] -- TODO: can ignore?

    ms = concatMap extrMName ds

    extrValDecl (A.VDecl d@(A.ValDecl _ _)) = [d]
    extrValDecl _                           = [] -- TODO: can ignore?

    vdcls = concatMap extrValDecl ds
 in
   DictDef{ddId=name, ddMethods=ms, ddDecls=vdcls}

{- TODO: consider following patterns:
            _ (A.ClassDecl (_, A.Tyvar _) _)
            _ (A.ClassDecl (_, A.Tycon _) _)
            _ (A.ClassDecl (_, A.FunTy _ _) _)
            _ (A.ClassDecl (_, A.AppTy (A.Tyvar _) _) _)
-}
trCdecl _ (A.ClassDecl (_, _) _) = error "trCDecl: unexpected"

renClassDecls :: [A.ClassDecl] -> RN [TempBind]
renClassDecls dcls = do
  tbss <- mapM
          (\(A.ClassDecl cls ds) -> do
              cname <- clsadd cls
              renDecls $ addvar cls $ suppDs ds cname)
          dcls
  return $ concat tbss
  where
    clsadd (_, A.AppTy (A.Tycon n) _) = do
      cname <- qname $ origName n
      st <- get
      let ce = rnCe st
          ce' = -- todo: super class
            fromMaybe (error $ "addClass failed: " ++ show (cname, ce))
            (addClass cname [] ce)
      put $ st{rnCe=ce'}
      return cname

    clsadd _ = error "Semant.renCDictdefDecls.clsadd"

    addvar c ds' = let (_, sigvar) = c
                       f (A.TypeSigDecl ns (_, sigdoc)) =
                         A.TypeSigDecl ns (Just sigvar, sigdoc)
                       f d = d
                   in map f ds'

{- suppDs -- Exstract TypeSigDictdefDecls and supplement ValDictdefDecls that defines
             overloaded functions.
-}
suppDs :: [A.Decl] -> Id -> [A.ValueDecl]
suppDs ds clsname =
  let
    ubNames [] cns cds' = (cns, cds')

    ubNames (A.VDecl cd@(A.TypeSigDecl ns _):ds'') cns cds' =
       ubNames ds'' (cns ++ map origName ns) (cd : cds')

    ubNames (A.VDecl (A.ValDecl _ _) : ds'') cns cds' = ubNames ds'' cns cds'

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

renInstDecls :: [A.InstDecl] -> RN ([TempBind], [(Id, Id)])
renInstDecls dcls' = do
  r <- mapM renInstDecl dcls'
  let (tbss, ctabs) = unzip r
  return (concat tbss, ctabs)
  where
    renInstDecl (A.InstDecl ctx t ds) = do
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
      let defds = ddDecls dict
          ds' = mergeDs ds (map A.VDecl defds)
      enterNewLevelWith $ "I%" ++ origName i -- see STG/isLocal
      (ds'', _, _) <- scanDecls ds'
      tbs <- renDecls ds''
      exitLevel
      return (tbs, (qin, qcn))

    instAdd :: [Pred] -> Pred -> RN ()
    instAdd ps p = do
      st <- get
      let ce = rnCe st
          ce' = fromMaybe (error $ "addInst failed: " ++ show (p, ce))
                (addInst ps p ce)
      put $ st{rnCe=ce'}

    extrId' (A.VDecl (A.ValDecl e _)) = origName $ extrName e
    extrId' _                         = error "extrId': unexpected"

    mergeDs ds1 ds2 =
      let
        names = map extrId' ds1
        ds2' = filter (\d -> extrId' d `notElem` names) ds2
      in
        ds1 ++ ds2'

    tops Nothing  = return []
    tops (Just x) = tops' x

    tops' (A.ParTy x) = tops' x

    tops' (A.AppTy (A.Tycon n1) (A.Tyvar n2)) = do
      qcn <- qname $ origName n1
      let x = TVar $ Tyvar (origName n2) Star -- TODO: always works?
      return [IsIn qcn x]

    tops' _ = error "tops': unexpected"

renDecls :: [A.ValueDecl] -> RN [TempBind]
renDecls decls = do tbss <- mapM renDecl decls
                    return $ concat tbss
  where
    renDecl (A.ValDecl expr rhs) = do
      enterNewLevel
      (n, pats) <- renFExp expr
      rexp      <- renRhs  rhs
      exitLevel
      return [(n, Nothing, [(pats, rexp)])]

    renDecl (A.TypeSigDecl ns (maybe_sigvar, sigdoc)) = do
      ns' <- mapM renameVar ns
      let kdict = kiExpr sigdoc []
      ps <- renSigvar maybe_sigvar kdict
      t <- renSigdoc sigdoc kdict
      return [(n, Just (ps :=> t), []) | n <- ns']

kiExpr :: A.Type -> [(Id, Kind)] -> [(Id, Kind)]

kiExpr (A.FunTy t1 t2) dict = kiExpr t2 $ kiExpr t1 dict

kiExpr (A.AppTy t1 t2) dict = dict ++ [(extrid t1, Kfun Star Star)
                                      ,(extrid t2, Star)]
  where extrid (A.Tyvar n) = origName n
        extrid _           = "extrid: unexpected"

kiExpr (A.Tyvar n) dict = dict ++ [(origName n, Star)]

kiExpr (A.ParTy e) dict = kiExpr e dict

kiExpr (A.Tycon _) dict = dict

kiExpr (A.ListTy e) dict = kiExpr e dict

kiExpr t dict = error $ "kiExpr: " ++ show (t, dict)

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
  "()"      -> return tUnit
  "Bool"    -> return tBool
  "Char"    -> return tChar
  "Integer" -> return tInteger
  "Int"     -> return tInt
  "IO"      -> return $ TCon (Tycon "IO" (Kfun Star Star))
  "String"  -> return tString
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

expandList :: [A.Exp] -> A.Exp
expandList = foldr (A.FunAppExp . A.FunAppExp aCons) aNil

renPat :: A.Exp -> RN Pat
renPat (A.VarExp n) | isConName n = do qn <- qname (origName n)
                                       x <- findCMs qn
                                       let a = fromMaybe
                                               (error $  "renPat(A.VarExp n) error: " ++ show n)
                                               x
                                       return $ PCon a []
                    | otherwise   = do qn <- renameVar n
                                       return $ PVar qn

renPat (A.ParExp e) = renPat e

renPat(A.InfixExp (A.InfixExp rest op2 e2) op1 e1) =
  resolveFixity rest op2 e2 op1 e1 >>= renPat

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

renPat (A.ListExp es) = renPat $ expandList es

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

resolveFixity :: A.Exp -> Name -> A.Exp -> Name -> A.Exp -> RN A.Exp
resolveFixity rest op2 e2 op1 e1 = do
  (prec1, fix1) <- lookupInfixOp op1
  (prec2, fix2) <- lookupInfixOp op2
  if prec1 == prec2 && (fix1 /= fix2 || fix1 == A.Infix)
    then fail "fixty resolution error."
    else if prec1 > prec2 || (prec1 == prec2 && fix1 == A.Infixr)
         then return (A.InfixExp rest op2 (opAppExp op1 e2 e1))
         else return (opAppExp op1 (A.InfixExp rest op2 e2) e1)
  where
    opAppExp op e = A.FunAppExp (A.FunAppExp (A.VarExp op) e)


renExp :: A.Exp -> RN Expr

renExp (A.InfixExp (A.InfixExp rest op2 e2) op1 e1) =
  resolveFixity rest op2 e2 op1 e1 >>= renExp

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
  (ds', _, _) <- scanDecls ds
  tbs <- renDecls ds'
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
    decl = A.VDecl $ A.ValDecl okp rhs
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
        ddecls = map
                 (\(A.Match p rhs) ->
                    A.VDecl $ A.ValDecl (A.FunAppExp (A.VarExp f) p) rhs) alts

renExp (A.ListExp es) =
  renExp $ expandList es

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
    decl = A.VDecl $ A.ValDecl okp rhs
    body = A.FunAppExp (A.FunAppExp aBind e) (A.VarExp ok)
    letexp = A.LetExp [decl] body

renExp (A.DoExp (A.LetStmt ddecls : stmts)) = renExp letexp
  where
    letexp = A.LetExp ddecls (A.DoExp stmts)

renExp (A.LamExp args e) = renExp (A.LetExp [decl] f)
  where f = A.VarExp $ Name "F" (0,0) False
        fexp = foldl' A.FunAppExp f args
        rhs = A.UnguardedRhs e []
        decl = A.VDecl $ A.ValDecl fexp rhs

renExp (A.LitExp (A.LitString s _)) = return $ Lit (LitStr s)

renExp (A.LitExp (A.LitInteger i _)) = return $ Lit (LitInt i)

renExp (A.LitExp (A.LitChar c _)) = return $ Lit (LitChar c)

renExp (A.ParExp e) = renExp e

-- pair
renExp (A.TupleExp [Just a, Just b]) = do
  e1 <- renExp a
  e2 <- renExp b
  let c = Const pairCfun
  return $ Ap (Ap c e1) e2

renExp e = error $ "Non-exhaustive patterns in renExp: " ++ show e
