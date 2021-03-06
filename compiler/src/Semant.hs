module Semant ( module Rename
              , renProg
--              , semProgram
              , semPrelude
              ) where

import qualified Absyn                      as A
import           BindGrouping
import           Rename
import           Symbol
import           Typing

import           Control.Monad
import           Control.Monad.State.Strict (get)
import qualified Data.Map.Strict            as Map
import           Debug.Trace

renProg ::
  A.Module
  -> RN ([BindGroup], [BindGroup], Assumps, [DictDef] ,[(Id, Id)])
renProg m = do
  let body = snd (A.body m)
      modid = fromModname $ A.modname m
  (ds0, cds, ids) <- scanDecls body
  let ds = if modid == "Main" then mainRestrict ++ ds0 else ds0
  ctbs <- renClassDecls cds
  let bgs' = toBg ctbs
      as2 = Map.fromList $ map (\(n, scm, _) -> (n, scm)) $ fst $ head bgs'
  let dicts = map (trCdecl modid) cds
  appendCDicts dicts
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

semPrelude :: A.Module -> RN ((Subst, Int, Assumps), [DictDef])
semPrelude m = do
  (bgs, _, as2, dicts, _) <- renProg m
  st <- get
  let ce = rnCe st
      as = rnCms st
      tiState = tiImportedProgram ce (Map.union as as2) bgs initialTI
  return (tiState, dicts)

