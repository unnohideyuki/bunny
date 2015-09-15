module Desugar where

import Symbol
import qualified Typing as Ty
import qualified Core

desModule :: Id -> Ty.Program -> [Ty.Assump] -> Core.Module
desModule modident bgs as =
  Core.Module modident [] []
