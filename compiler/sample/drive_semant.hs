module Main where

import Parser
import qualified Absyn
import Semant
import Symbol

do_semant :: Absyn.Module -> IO ()
do_semant m = do
  let lv = initialLevel $ Absyn.modid m
  print lv
  let body = snd $ Absyn.body m
  -- print body
  let result = collectTopNames (lv_prefix lv) (empty, empty) body
  print result

main :: IO ()
main = do
  s <- getContents
  let r = parse s
  case r of
    Left  mes -> putStrLn $ "Error: " ++ mes
    Right m -> do do_semant m
                  
