module Desugar where

import           Core
import           PreDefined
import           Symbol
import           TrCore
import qualified Typing     as Ty

dsgModule :: Id -> Ty.Program -> [Ty.Assump] -> ConstructorInfo -> Module
dsgModule modident bgs as ci =
  let
    [(es, iss)] = bgs
    is = concat iss
    es' = map (\(n, _, alts) -> (n, alts)) es
    vdefs = dsgBs [] (es' ++ is) ci
    b = translateVdefs as vdefs ci
  in
   Core.Module modident [b]

