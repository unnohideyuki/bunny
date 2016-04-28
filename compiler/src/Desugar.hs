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
    bs = trace (show (is, vdefs)) $ map (trBind as) vdefs
  in
   trace (show bs) Core.Module modident bs

trBind :: [Ty.Assump] -> (Id, Expression) -> Core.Bind
trBind as (n, e) =
  let
    v = TermVar n (tyLookup n as)
    e' = trExpr as e
  in
   NoRec v e'

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

ptypes :: Ty.Type -> [Ty.Type]
ptypes t =
 let
   ptypes' ts (Ty.TAp
               (Ty.TAp
                (Ty.TCon (Ty.Tycon "(->)" (Ty.Kfun Ty.Star (Ty.Kfun Ty.Star Ty.Star))))
                t1)
               t2)
     = ptypes' (ts ++ [t1]) t2
   ptypes' ts _ = ts
 in
  ptypes' [] t
  


