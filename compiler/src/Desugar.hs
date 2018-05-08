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
    is' = map (\(n, _, alts) -> (n, alts)) es
    vdefs = dsgIs [] (is ++ is') ci
    b = translateVdefs as vdefs ci
  in
   Core.Module modident [b]

