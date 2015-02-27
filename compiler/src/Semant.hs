module Semant where

import Control.Monad.State (State, state, runState)
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

-- collect top-level names

collectTopNames :: Id -> (Table Id, Table FixtyInfo) -> [A.Decl]
                   -> (Table Id, Table FixtyInfo)
collectTopNames _     (rdict, ifxenv) [] = (rdict, ifxenv)
collectTopNames modid (rdict, ifxenv) (decl:decls) =
  let
    (rdict', ifxenv') = collname decl

    qname = (++) (modid ++ ".")

    renNoCheck name =
      let
        n = orig_name name
        rdict'' = insert n (qname n) rdict
      in
       (rdict'', ifxenv)

    extrName (A.VarExp name)       = name
    extrName (A.FunAppExp f _)     = extrName f
    extrName (A.InfixExp _ name _) = name
    extrName e                     = error $ "unexpected exp:" ++ show e

    collname (A.ValDecl e _) = renNoCheck $ extrName e

    collname _ = (rdict, ifxenv)
  in
   collectTopNames modid (rdict', ifxenv') decls

-- transProg

data RenamingState = RenamingState { ren_modid  :: Id
                                   , ren_lvs    :: [Level]
                                   , ren_rdict  :: Table Id
                                   , ren_ifxenv :: Table Id
                                   }

type TempBinds = (Id, Maybe Scheme, [Alt])

