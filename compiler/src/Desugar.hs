module Desugar where

import Symbol
import qualified Typing as Ty
import qualified Types as Ty
import Core
import Pattern
import TrCore

import Debug.Trace

dsgModule :: Id -> Ty.Program -> [Ty.Assump] -> Module
dsgModule modident bgs as =
  let
    [(es, iss)] = bgs
    [is] = iss
    vdefs = dsgIs [] is
    b = trace (show (is, vdefs)) $ translateVdefs as vdefs
  in
   trace (show b) Core.Module modident [b]

