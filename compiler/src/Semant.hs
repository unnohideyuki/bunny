module Semant ( module Rename
              , semProgram
              , semPrelude
              ) where

import qualified Absyn                      as A
import           BindGrouping
import           Rename
import           Symbol
import           Typing

import           Control.Monad.State.Strict (get)
import           Debug.Trace

renProg ::
  A.Module
  -> RN ([BindGroup], [BindGroup], [Assump], [DictDef] ,[(Id, Id)])
renProg m = do
  let body = snd (A.body m)
      modid = fromModname $ A.modname m
  (ds0, cds, ids) <- scanDecls body
  let ds = if modid == "Main" then mainRestrict ++ ds0 else ds0
  ctbs <- renClassDecls cds
  let bgs' = toBg ctbs
      as2 = map (\(n, scm, _) -> n :>: scm) $ fst $ head bgs'
  let dicts = map (trCdecl modid) cds
  putCDicts dicts
  (itbs, ctab) <- renInstDecls ids
  tbs <- renDecls ds
  -- NOTE#1: followings are not clear! see the note page 233.
  let bgs = toBg $ tbs ++ itbs
      bgs'' = toBg $ ctbs ++ tbs ++ itbs
  return (bgs, bgs'', as2, dicts, ctab)
  where -- type restriction for main,
        -- main# :: IO ()
        -- main# = main >> return ()
        mainRestrict =
          [ (A.TypeSigDecl
              [Name "main#" (0,0) False]
              (Nothing,
               A.AppTy (A.Tycon (Name "IO" (0,0) True)) (A.Tycon (Name "()" (0,0) True))))
          , (A.ValDecl
              (A.VarExp (Name "main#" (0,1) False))
              (A.UnguardedRhs
                (A.InfixExp (A.VarExp (Name "main" (0,0) False))
                            (Name ">>" (0,0) False)
                            (A.FunAppExp (A.VarExp (Name "return" (0,0) False))
                                         (A.VarExp (Name "()" (0,0) True)))) []))
          ]


semProgram :: A.Module -> (Subst, Int, [Assump])
           -> RN ([BindGroup], [Assump], [DictDef] ,[(Id, Id)], ClassEnv)
semProgram m cont = do
  (bgs, bgs'', as2, dicts, ctab) <- renProg m
  st <- get
  let ce = rnCe st
      as = rnCms st
  let as' = tiProgram ce (as ++ as2) bgs cont
  return (bgs'', as' ++ as2, dicts, ctab, ce)

semPrelude :: A.Module -> RN (Subst, Int, [Assump])
semPrelude m = do
  (bgs, _, as2, _, _) <- renProg m
  st <- get
  let ce = rnCe st
      as = rnCms st
  return $ tiImportedProgram ce (as ++ as2) bgs initialTI

