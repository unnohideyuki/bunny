module Desugar where

import Symbol
import qualified Typing as Ty
import Core
import TrCore

dsgModule :: Id -> Ty.Program -> [Ty.Assump] -> Module
dsgModule modident bgs as =
  let
    [(es, iss)] = bgs
    is = concat iss
    is' = map (\(n, _, alts) -> (n, alts)) es
    vdefs = dsgIs [] (is ++ is')
    b = translateVdefs as vdefs
  in
   Core.Module modident [b]

