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
  (ds, cds, ids) <- scanDecls body
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

semProgram :: A.Module -> (Subst, Int, [Assump])
           -> RN ([BindGroup], [Assump], [DictDef] ,[(Id, Id)])
semProgram m cont = do
  (bgs, bgs'', as2, dicts, ctab) <- renProg m
  st <- get
  let ce = rnCe st
      as = rnCms st
  let as' = tiProgram ce (as ++ as2) bgs cont
  return (bgs'', as' ++ as2, dicts, ctab)

semPrelude :: A.Module -> RN (Subst, Int, [Assump])
semPrelude m = do
  (bgs, _, as2, _, _) <- renProg m
  st <- get
  let ce = rnCe st
      as = rnCms st
  return $ tiImportedProgram ce (as ++ as2) bgs initialTI

