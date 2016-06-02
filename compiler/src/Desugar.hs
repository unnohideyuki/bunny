module Desugar where

import Symbol
import qualified Typing as Ty
import qualified Types as Ty
import Core
import Pattern
import TrCore

dsgModule :: Id -> Ty.Program -> [Ty.Assump] -> Module
dsgModule modident bgs as =
  let
    [(es, iss)] = bgs
    [is] = iss
    vdefs = dsgIs [] is
    b = translateVdefs as vdefs
  in
   Core.Module modident [b]

