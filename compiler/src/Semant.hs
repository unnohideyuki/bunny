module Semant where

import Control.Monad.State (State, state)
import Symbol
import qualified Absyn as A
import Typing

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

type RN a = State (Id, [Level], Table Id, Table FixtyInfo) a

renameVar :: Name -> RN ()
renameVar name = state $ \(modid, (lv:lvs), tenv, ifxenv) ->
  let
    prefix = lv_prefix lv
    qname = (++) (prefix ++ ".")
    n = orig_name name
    dict' = insert n (qname n) (lv_dict lv)
    lv' = lv{lv_dict=dict'}
  in
   ((), (modid, (lv':lvs), tenv, ifxenv))


collectNames :: [A.Decl] -> RN ()
collectNames [] = return ()
collectNames (decl:decls) = do collname decl; collectNames decls
  where
    extrName (A.VarExp name)       = name
    extrName (A.FunAppExp f _)     = extrName f
    extrName (A.InfixExp _ name _) = name
    extrName e                     = error $ "unexpected exp:" ++ show e

    collname (A.ValDecl e _) = renameVar (extrName e)
    collname _               = return ()

type TempBinds = (Id, Maybe Scheme, [Alt])

