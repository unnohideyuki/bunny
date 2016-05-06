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
    bs = trace (show (is, vdefs)) $ translateVdefs as vdefs
  in
   trace (show bs) Core.Module modident bs

dsgIs vds [] = vds
dsgIs vds (impl:is) = dsgIs (desis impl:vds) is
  where
    desis (n, alts) = (n, dsgAlts $ cnvalts alts)

dsgAlts alts@((pats,_):_) =
  let
    k = length pats
    us = [mkVar i | i <- [1..k]]
    e = match k us alts Error
  in
   Lambda us e

cnvalts alts =
  fmap (\(pats, e) -> (pats, OtherExpression e)) alts

