module Main where

import Parser
import qualified Absyn
import Semant
import Symbol
import PreDefined
import Core
import Desugar

import Control.Monad.State.Strict (runState)

do_semant :: Absyn.Module -> IO ()
do_semant m = do
  -- TODO: regular way to add primitive names.
  let lv = (initialLevel $ Absyn.modid m){lv_dict=primNames}
  let st = RnState (lv_prefix lv) [lv] empty empty preludeClasses primConsMems [] [] empty []
      ((bgs, as, _, _), st') = runState (renProg m) st
  -- print st'
  -- print bgs
  print as
  let cmod = dsgModule (rn_modid st') bgs (as ++ primConsMems)
  putStrLn $ show $ ppModule cmod
  putStrLn "\nok."

main :: IO ()
main = do
  s <- getContents
  let r = parse s
  case r of
    Left  mes -> putStrLn $ "Error: " ++ mes
    Right m -> do do_semant m

