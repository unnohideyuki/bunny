module Desugar where

import Symbol
import qualified Typing as Ty
import qualified Core
import Pattern
import TrCore

import Debug.Trace

dsgModule :: Id -> Ty.Program -> [Ty.Assump] -> Core.Module
dsgModule modident bgs as =
  let
    [(es, iss)] = bgs
    [is] = iss
    vdefs = dsgIs [] is
    bs = map trBind vdefs
  in
   trace (show vdefs) Core.Module modident bs

trBind :: (Id, Expression) -> Core.Bind
trBind (n, e) = let
  n = 

dsgIs vds [] = vds
dsgIs vds (impl:is) = dsgIs (desis impl:vds) is
  where
    desis (n, alts) = (n, dsgAlts $ cnvalts alts)

dsgAlts alts@((pats,_):_) =
  let
    k = length pats
    us = [mkVar i | i <- [1..k]]
  in
   match k us alts Error

cnvalts alts =
  fmap (\(pats, e) -> (pats, OtherExpression e)) alts
  
