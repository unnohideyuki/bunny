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

import           Control.Exception          (assert)
import           Control.Monad              (mapM, when)
import           Control.Monad.State.Strict (get, put)
import           Data.List                  (concatMap, foldl', notElem, sort)
import           Data.Maybe                 (fromMaybe)
import           Debug.Trace

scanDecls :: [A.Decl] -> RN ([A.ValueDecl], [A.ClassDecl], [A.InstDecl])
scanDecls ds = do
  r <- mapM scandecl ds
  let (dss, cdss, idss) = unzip3 r
      ds = concat dss
      cds = concat cdss
      ids = concat idss
  ds' <- concat <$> mapM scanValueDecl2 ds
  cds' <- mapM scanClassDecl2 cds
  ids' <- mapM scanInstDecl2 ids
  return (ds', cds', ids')
  where
    scanValueDecl2 (A.ValDecl (A.AsPat n expr) rhs) = do
      ds <- trAsPat n expr rhs
      ds' <- concat <$> mapM scanValueDecl2 ds
      return ds'
        where trAsPat n (A.ParExp e) rhs = trAsPat n e rhs
              trAsPat n (A.FunAppExp (A.FunAppExp c a) b) rhs = do
                let d1 = A.ValDecl (A.VarExp n) rhs
                    a1 = A.VarExp (Name "_a1" (0,0) False)
                    a2 = A.VarExp (Name "_a2" (0,0) False)
                    cab = A.FunAppExp (A.FunAppExp c a1) a2
                    e2 = A.CaseExp (A.VarExp n) [A.Match cab (A.UnguardedRhs a1 [])]
                    e3 = A.CaseExp (A.VarExp n) [A.Match cab (A.UnguardedRhs a2 [])]
                    d2 = A.ValDecl a (A.UnguardedRhs e2 [])
                    d3 = A.ValDecl b (A.UnguardedRhs e3 [])
                  in return [d1, d2, d3]
              trAsPat n (A.TupleExp [Just a, Just b]) rhs = do
                let d1 = A.ValDecl (A.VarExp n) rhs
                    a1 = A.VarExp (Name "_a1" (0,0) False)
                    a2 = A.VarExp (Name "_a2" (0,0) False)
                    cab = A.TupleExp [Just a1, Just a2]
                    e2 = A.CaseExp (A.VarExp n) [A.Match cab (A.UnguardedRhs a1 [])]
                    e3 = A.CaseExp (A.VarExp n) [A.Match cab (A.UnguardedRhs a2 [])]
                    d2 = A.ValDecl a (A.UnguardedRhs e2 [])
                    d3 = A.ValDecl b (A.UnguardedRhs e3 [])
                  in return [d1, d2, d3]

    scanValueDecl2 (A.ValDecl expr rhs) = do
      expr' <- removeInfix expr
      renameVar (extrName expr')
      return [A.ValDecl expr' rhs]

    scanValueDecl2 e = return [e]

    scanClassDecl2 (A.ClassDecl hdr ds) = do
      dss' <- mapM (\(A.VDecl d) -> scanValueDecl2 d) ds
      let ds' = map A.VDecl $ concat dss'
      return (A.ClassDecl hdr ds')

    scanInstDecl2 (A.InstDecl ctx t ds) = do
      dss' <- mapM (\(A.VDecl d) -> scanValueDecl2 d) ds
      let ds' = map A.VDecl $ concat dss'
      return (A.InstDecl ctx t ds')

    removeInfix :: A.Exp -> RN A.Exp
    removeInfix e@(A.VarExp n) = return e

    removeInfix (A.InfixExp (A.InfixExp rest op2 e2) op1 e1) = do
      e' <- resolveFixity rest op2 e2 op1 e1
      removeInfix e'

    removeInfix (A.InfixExp le op re) =
      removeInfix (A.FunAppExp (A.FunAppExp (A.VarExp op) le) re)

    removeInfix (A.FunAppExp f e) = do f' <- removeInfix f
                                       e' <- removeInfix e
                                       return (A.FunAppExp f' e')

    removeInfix (A.ParExp e) = removeInfix e

    removeInfix (A.TupleExp es) = do es' <- mapM (\me -> case me of
                                                  Just e -> do e' <- removeInfix e
                                                               return (Just e')
                                                  Nothing -> return Nothing)
                                         es
                                     return (A.TupleExp es')

    removeInfix e@(A.LitExp _) = return e
    removeInfix A.WildcardPat = return A.WildcardPat

    removeInfix e@(A.ListExp es) = removeInfix $ expandList es

    removeInfix e = error $ "removeInfix :" ++ show e

    scandecl (A.VDecl d@(A.ValDecl e _)) = do return ([d], [], [])

    scandecl (A.VDecl d@(A.TypeSigDecl _ _)) = return ([d], [], [])

    scandecl (A.CDecl d@(A.ClassDecl (_, A.AppTy (A.Tycon n) _) ds')) = do
      _ <- renameVar n
      mapM_ colname' ds'
      return ([], [d], [])
      where colname' (A.VDecl (A.ValDecl e _))      = return ()
              -- renameVar (extrName e) >> return ()
            colname' (A.VDecl (A.TypeSigDecl ns _)) = mapM_ renameVar ns

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
      (tn, tvs) <- parseTy ty
      qtn <- renameVar tn
      let k = foldr Kfun Star (replicate (length tvs) Star)
          t = foldl TAp (TCon (Tycon qtn k)) tvs

      -- the keys of TypeConst dict should be qualified?
      appendTConst (origName tn) (TCon (Tycon qtn k))

      cs <- mapM (\(A.Con t) -> parseTy t) consts
      let renCs (n, ts) = do
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
        parseTy (A.Tycon n) = return (n, [])
        parseTy t = parsety' [] t
          where
            parsety' tvs (A.AppTy (A.Tycon cn) t2) = do rt2 <- renTy t2
                                                        return (cn, rt2 : tvs)
            parsety' tvs (A.AppTy t t2) = do rt2 <- renTy t2
                                             parsety' (rt2 : tvs) t

        renTy (A.Tyvar i) = return $ TVar (Tyvar (origName i) Star)

        renTy (A.Tycon i) = do
          t <- lookupTConst (origName i)
          st <- get
          return $ fromMaybe
            (error $ "Non-exhaustive patterns: " ++ origName i ++ (show $ rnTConsts st))
            t

        renTy (A.ListTy t) = do rt <- renTy t
                                return $ list rt

        parseConsts (n, ts) = do
          qn <- renameVar n
          let aty = length ts
          return (qn, aty)

    scandecl (A.DefaultDecl _)   = error "not yet: DefaultDecl"
    scandecl (A.ForeignDecl _)   = error "not yet: ForeignDecl"
    scandecl (A.SynonymDecl _ _) = error "not yet: SynonymDecl"
    scandecl A.NewtypeDecl{}     = error "not yet: NewtypeDecl"

trCdecl :: Id -> A.ClassDecl -> DictDef
trCdecl modid (A.ClassDecl (_, sigvar@(A.AppTy (A.Tycon n) _)) ds) =
  let
    name = modid ++ "." ++ origName n

    extrMName (A.VDecl (A.TypeSigDecl ns _)) = map origName ns
    extrMName _                              = []
    ms = concatMap extrMName ds

    extrValDecl (A.VDecl d@(A.ValDecl _ _)) = [d]
    extrValDecl _                           = []
    vdcls = concatMap extrValDecl ds

    extrTSygDecl (A.VDecl (A.TypeSigDecl ns (_, sigdoc))) =
      [A.TypeSigDecl ns (Just sigvar, sigdoc)]
    extrTSygDecl _                               = []
    tdcls = concatMap extrTSygDecl ds
 in
   DictDef{ddId=name, ddMethods=ms, ddDecls=vdcls, ddTDecls=tdcls}

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
    extr_sc Nothing                                  = []
    extr_sc (Just (A.ParTy (A.AppTy (A.Tycon i) _))) = [origName i]
    extr_sc (Just t) = error $ "extr_sc: " ++ show t

    clsadd (maybe_sc, A.AppTy (A.Tycon n) _) = do
      cname <- qname $ origName n
      sps <- mapM qname (extr_sc maybe_sc)
      st <- get
      let ce = rnCe st
          ce' = fromMaybe (error $ "addClass failed: " ++ show (cname, ce))
                (addClass cname sps ce)
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
                                    (A.LitExp (A.LitString clsname (0,0)))
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
      let parseTy' (A.Tycon n2) as = (n2, as)
          parseTy' (A.AppTy t1 (A.Tyvar n)) as = parseTy' t1 (origName n:as)
          parseTy' (A.ParTy t) as = parseTy' t as
          parseTy' x _ = error $ "parseTy': " ++ show x

      (qcn, qin, i, as) <- case t of
        (A.AppTy (A.Tycon n1) (A.ListTy (A.Tyvar n2))) -> do
          qcn <- qname $ origName n1
          return (qcn, "Prelude.[]", nNil, [origName n2])
        (A.AppTy (A.Tycon n1) (A.TupleTy vs)) -> do
          qcn <- qname $ origName n1
          let n2 = Name ("(" ++ replicate (length vs - 1) ',' ++ ")") (0,0) True
              as = map (\(A.Tyvar n) -> origName n) vs
          qin <- renameVar n2
          return (qcn, qin, n2, as)
        (A.AppTy (A.Tycon n1) t') -> do let (n2, as) = parseTy' t' []
                                        qcn <- qname $ origName n1
                                        qin <- renameVar n2
                                        return (qcn, qin, n2, as)
        _ -> error $ "Non-exhaustive pattern in case: " ++ show t

      k0 <- lookupKdict qcn
      let k = if null as
              then k0
              else foldr Kfun k0 (replicate (length as) Star)
          as' = map (\a -> TVar (Tyvar a Star)) as
          p = IsIn qcn (foldl' TAp (TCon (Tycon qin k)) as')

      ps <- tops ctx
      instAdd ps p
      dict <- lookupCDicts qcn
      let defds = ddDecls dict
          ds' = mergeDs ds (map A.VDecl defds)
      ds'' <- concat <$> mapM (renMDecl (origName i ++ "%I")) ds'
      tsdecls <-
        concat <$> mapM (renTDecl (origName i ++ "%I") t ctx) (ddTDecls dict)
      tbs <- renDecls (tsdecls ++ ds'')
      return (tbs, (qin, qcn))

    renTDecl :: Id -> A.Type -> Maybe A.Type -> A.ValueDecl -> RN [A.ValueDecl]
    renTDecl pfx (A.AppTy _ tc) osv (A.TypeSigDecl ns (sigvar,sigdoc)) =
      do ns' <- mapM (ren' pfx) ns
         let Just (A.AppTy _ (A.Tyvar tv)) = sigvar
             a = origName tv

             subst' t@(A.Tyvar name) | origName name == a = tc
                                     | otherwise          = t
             subst' t@(A.Tycon _) = t
             subst' (A.FunTy t1 t2) = A.FunTy (subst' t1) (subst' t2)
             subst' (A.AppTy t1 t2) = A.AppTy (subst' t1) (subst' t2)
             subst' (A.BangTy t) = A.BangTy (subst' t)
             subst' (A.TupleTy ts) = A.TupleTy $ map subst' ts
             subst' (A.ListTy t) = A.ListTy (subst' t)
             subst' (A.ParTy t) = subst' t
             subst' t@(A.RecTy _) = t

             sigdoc' = subst' sigdoc

             d' = A.TypeSigDecl ns' (osv, sigdoc')
         return [d']

    renTDecl pfx _ _ d = return [] -- not implemented yet.

    renMDecl :: Id -> A.Decl -> RN [A.ValueDecl]
    renMDecl pfx (A.VDecl d) = do d' <- renMName pfx d
                                  return [d']
    renMDecl _   _           = return []

    renMName :: Id -> A.ValueDecl -> RN A.ValueDecl
    renMName pfx (A.ValDecl expr rhs) = do expr' <- renmname expr
                                           return (A.ValDecl expr' rhs)
      where
        renmname (A.VarExp n)          = do n' <- ren' pfx n
                                            return (A.VarExp n')
        renmname (A.FunAppExp f e)     = do f' <- renmname f
                                            return (A.FunAppExp f' e)
        renmname (A.InfixExp le op re) = do op' <- ren' pfx op
                                            return (A.InfixExp le op' re)

    renMName pfx (A.TypeSigDecl _ _) = error "must not occur"

    ren' pfx name = do
      lv:lvs <- getLvs
      let n = origName name
          n' = lvPrefix lv ++ "." ++ pfx ++ "." ++ n
          dict' = insert n' n' (lvDict lv) -- here is defferent from renameVar
          lv' = lv{lvDict=dict'}
      putLvs (lv':lvs)
      return name{origName=n'}

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

    tops (Just (A.TupleTy ts)) = mapM tops' ts
    tops (Just t)              = mapM tops' [t]
    tops Nothing               = return []

    tops' (A.ParTy x) = tops' x

    tops' (A.AppTy (A.Tycon n1) (A.Tyvar n2)) = do
      qcn <- qname $ origName n1
      let x = TVar $ Tyvar (origName n2) Star -- TODO: always works?
      return (IsIn qcn x)

    tops' t = error $ "tops': unexpected:" ++ show t

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

kiExpr t@(A.AppTy _ _) dict = dict ++ kiexpr' t []
  where kiexpr' (A.AppTy t1 (A.Tyvar n)) ds = kiexpr' t1 ((origName n, Star):ds)
        kiexpr' (A.Tycon _) ds = ds
        kiexpr' (A.Tyvar n) ds =
          (origName n, foldr Kfun Star (replicate (length ds) Star)):ds

kiExpr (A.Tyvar n) dict     = dict ++ [(origName n, Star)]

kiExpr (A.ParTy e) dict     = kiExpr e dict

kiExpr (A.Tycon _) dict     = dict

kiExpr (A.ListTy e) dict    = kiExpr e dict

kiExpr (A.TupleTy ts) dict  = dict ++ concatMap (\t -> kiExpr t []) ts

kiExpr t dict               = error $ "kiExpr: " ++ show (t, dict)

renSigvar :: Maybe A.Type -> [(Id, Kind)] -> RN [Pred]
renSigvar Nothing _ = return []
renSigvar (Just (A.AppTy (A.Tycon n) (A.Tyvar m))) kdict = do
  qn <- qname $ origName n
  let vname = origName m
      k = kindLookup vname kdict
  when (isConName n) (insertKdict qn k)
  return [IsIn qn (TVar (Tyvar vname k))]

renSigvar (Just (A.ParTy t)) kdict = renSigvar (Just t) kdict

renSigvar (Just (A.TupleTy ts)) kdict = do
  pss <- mapM (\t -> renSigvar (Just t) kdict) ts
  return $ concat pss

renSigvar x y = error $ "renSigvar: " ++ show (x,y)

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

renSigdoc (A.Tycon n) _ = do
  let n' = origName n
  t <- lookupTConst n'
  return $ fromMaybe (error $ "renSigDoc $ A.Tycon " ++ n') t

renSigdoc (A.ListTy e) kdict = do
  t <- renSigdoc e kdict
  return $ list t

renSigdoc (A.TupleTy ts) kdict = do
  let len = length ts
      tcname = "Prelude.(" ++ replicate (len - 1) ',' ++ ")"
      tckind = foldr Kfun Star (replicate len Star)
      tc = TCon (Tycon tcname tckind)
  ts' <- mapM (\t -> renSigdoc t kdict) ts
  let r = foldl' TAp tc ts'
  return r

renSigdoc t _ = error $ "renSigdoc" ++ show t

kindLookup :: Id -> [(Id, Kind)] -> Kind
kindLookup n kdict =
  fromMaybe (error $ "Kind not infered " ++ show (n, kdict)) (lookup n kdict)


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

renFExp e = error $ "renFExp: " ++ show e

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

renPat (A.InfixExp (A.InfixExp rest op2 e2) op1 e1) =
  resolveFixity rest op2 e2 op1 e1 >>= renPat

renPat (A.InfixExp e2 op e1) =
  renPat (A.FunAppExp (A.FunAppExp (A.VarExp op) e2) e1)

renPat (A.FunAppExp f f') = renPCon (A.FunAppExp f f') []
  where renPCon (A.FunAppExp (A.FunAppExp e e') e'') pats = do
          pat <- renPat e''
          renPCon (A.FunAppExp e e') (pat:pats)
        renPCon (A.FunAppExp (A.VarExp n) e') pats = do
          qn <- qname (origName n)
          maybe_a <- findCMs qn
          let a = case maybe_a of
                Just a' -> a'
                Nothing -> error $ "findCMs failed: " ++ show qn
          pat' <- renPat e'
          return $ PCon a (pat':pats)
        renPCon _ _ = error "renPCon: unexpected"

renPat (A.TupleExp [Just e1, Just e2]) = do
  p1 <- renPat e1
  p2 <- renPat e2
  return $ PCon pairCfun [p1, p2]

renPat (A.ListExp es) = renPat $ expandList es

renPat A.WildcardPat = return PWildcard

renPat (A.LitExp (A.LitString s _)) = renPat (lit2exp s)
  where lit2exp []     = A.VarExp (Name "[]" (0,0) True)
        lit2exp (c:cs) = A.InfixExp
                         (A.LitExp (A.LitChar c (0,0)))
                         (Name ":" (0,0) True)
                         (lit2exp cs)

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

renRhs (A.GuardedRhs gs decls) =
  let err = A.FunAppExp (A.VarExp (Name {origName="error", namePos=(0,0), isConName = False}))
                        (A.LitExp (A.LitString "Non-exaustive patterns" (0,0)))
      cnvGs []                         = err
      -- todo cnvGs support most simple case that has only one statement.
      cnvGs (([A.ExpStmt e1], e2):gs') = A.IfExp e1 e2 (cnvGs gs')
  in renExp $ A.LetExp decls (cnvGs gs)

renRhs rhs = do
  st <- get
  error $ "renRhs not yet implemented. " ++ show rhs

resolveFixity :: A.Exp -> Name -> A.Exp -> Name -> A.Exp -> RN A.Exp
resolveFixity rest op2 e2 op1 e1 = do
  (prec1, fix1) <- lookupInfixOp op1
  (prec2, fix2) <- lookupInfixOp op2
  -- trace (show (op1, prec1, op2, fix1, prec2, fix2)) $ return ()
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
  assert (length bgs == 1) $ return (Let (head bgs) e') -- TODO: (head bgs) is temporary

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

renExp (A.UMinusExp e) = do
  let f_negate = A.VarExp (Name {origName = "negate", namePos = (0, 0), isConName = False})
  renExp (A.FunAppExp f_negate e)

renExp (A.ExpWithTySig e sig) =
  let x_name = (Name "x" (0,0) False)
      x_valdecl = A.VDecl (A.ValDecl (A.VarExp x_name) (A.UnguardedRhs e []))
      x_tysigdecl = A.VDecl (A.TypeSigDecl [x_name] sig)
  in renExp (A.LetExp [x_tysigdecl, x_valdecl] (A.VarExp x_name))

renExp (A.SectionR opname e) =
  let x_var = A.VarExp (Name "x" (0,0) False)
      e_body = A.InfixExp x_var opname e
  in renExp (A.LamExp [x_var] e_body)

renExp (A.SectionL e opname) =
  let x_var = A.VarExp (Name "x" (0,0) False)
      e_body = A.InfixExp e opname x_var
  in renExp (A.LamExp [x_var] e_body)

renExp (A.ArithSeqExp (A.From n)) =
  renExp (A.FunAppExp (A.VarExp (Name "enumFrom" (0,0) False)) n)

renExp (A.ArithSeqExp (A.FromThen n n')) =
  renExp (A.FunAppExp (A.FunAppExp (A.VarExp (Name "enumFromThen" (0,0) False)) n) n')

renExp (A.ArithSeqExp (A.FromTo n m)) =
  renExp (A.FunAppExp (A.FunAppExp (A.VarExp (Name "enumFromTo" (0,0) False)) n) m)

renExp (A.ArithSeqExp (A.FromThenTo n n' m)) =
  renExp (A.FunAppExp (A.FunAppExp (A.FunAppExp
                                    (A.VarExp (Name "enumFromThenTo" (0,0) False)) n) n') m)

renExp e = error $ "Non-exhaustive patterns in renExp: " ++ show e
