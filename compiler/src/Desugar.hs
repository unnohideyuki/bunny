module Desugar where

import Symbol
import qualified Typing as Ty
import qualified Core
import Pattern

import Debug.Trace

desModule :: Id -> Ty.Program -> [Ty.Assump] -> Core.Module
desModule modident bgs as =
  let
    [(es, iss)] = bgs
    [is] = iss
    vdefs = desIs [] is
  in
   trace (show vdefs) Core.Module modident [] []

desIs vds [] = vds
desIs vds (impl:is) = desIs (desis impl:vds) is
  where
    desis (n, alts) = (n, desAlts $ cnvalts alts)

desAlts alts@((pats,_):_) =
  let
    k = length pats
    us = [mkVar i | i <- [1..k]]
  in
   match k us alts Error

cnvalts alts =
  fmap (\(pats, e) -> (pats, OtherExpression e)) alts
  