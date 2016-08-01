module Main where

import Parser
import qualified Absyn
import Semant
import Symbol
import PreDefined
import Core
import Desugar
import qualified TrSTG as TR
import qualified CodeGen

import Control.Monad.State.Strict (runState)

do_compile :: Absyn.Module -> IO ()
do_compile m = do
  -- TODO: regular way to add primitive names.
  let lv = (initialLevel $ Absyn.modid m){lv_dict=primNames}
  let st = RnState (lv_prefix lv) [lv] empty empty preludeClasses primConsMems [] [] empty Nothing
      ((bgs, as), st') = runState (renProg m) st
  let cmod = dsgModule (rn_modid st') bgs (as ++ primConsMems)
  let b = case cmod of
        Module _ [b'] -> b'
  let b' = TR.trBind b
  CodeGen.emitProgram b' "" "" -- dummy
    

main :: IO ()
main = do
  s <- getContents
  let r = parse s
  case r of
    Left  mes -> putStrLn $ "Error: " ++ mes
    Right m -> do do_compile m

