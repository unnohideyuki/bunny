module Main where

import Parser
import qualified Absyn
import Semant
import Symbol
import PreDefined

import Control.Monad.State.Strict (runState)

do_semant :: Absyn.Module -> IO ()
do_semant m = do
  -- TODO: regular way to add primitive names.
  let lv = (initialLevel $ Absyn.modid m){lv_dict=primNames}
  let st = RnState (lv_prefix lv) [lv] empty empty preludeClasses primConsMems [] [] empty Nothing
      ((bgs, _, _), st') = runState (renProg m) st
  print bgs

main :: IO ()
main = do
  s <- getContents
  let r = parse s
  case r of
    Left  mes -> putStrLn $ "Error: " ++ mes
    Right m -> do do_semant m
                  
