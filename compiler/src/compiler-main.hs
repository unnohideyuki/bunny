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
import DictPass (tcBind)
import Typing (initialEnv, initialTI, Subst, Assump)

import CompilerOpts
import DDumpAssump
import DDumpCore

import Options.Applicative
import Control.Monad.State.Strict (runState)
import System.IO
import Control.Monad

get_init_rnstate :: RnState
get_init_rnstate =
  let
    ifxenv = insert "Prim.:" (RightAssoc 5) Symbol.empty
  in
   RnState { rn_modid = ""
           , rn_lvs = []
           , rn_tenv = Symbol.empty
           , rn_ifxenv = ifxenv
           , rn_ce = initialEnv -- preludeClasses
           , rn_cms = primConsMems
           , rn_tbs = []
           , rn_tbstack = []
           , rn_kdict = Symbol.empty
           , rn_cdicts = []
           }

ti_as :: (a, b, c) -> c
ti_as (_, _, as) = as

debugmes :: Bool -> String -> IO ()
debugmes verbose_mode message = when verbose_mode $ hPutStr stderr message

implicit_prelude :: String -> Bool -> IO ((Subst, Int, [Assump]), RnState)
implicit_prelude prelude_dir verbose_mode = do
  debugmes verbose_mode "implicit_prelude ... "
  let src = prelude_dir ++ "/Prelude.hs"
  h <- openFile src ReadMode
  s <- hGetContents h
  case parse s of
    Left mes -> error $ "Error: " ++ mes
    Right m -> do debugmes verbose_mode "done.\n"
                  return $ do_implicit_prelude m
  where
    do_implicit_prelude m = 
      let  
        st0 = get_init_rnstate
        lv = (initialLevel $ Absyn.modid m){lv_dict=primNames}
        st = st0{rn_modid = (lv_prefix lv), rn_lvs = [lv]}
        (cont, rnstate) = runState (renPrelude m) st
        as = rn_cms rnstate
        as' = ti_as cont
      in (cont, rnstate{rn_cms = as ++ as'})

do_compile :: RnState -> Absyn.Module -> String -> (Subst, Int, [Assump])
              -> Options -> IO ()
do_compile st0 m dest cont opts = do
  let verbose_mode = opt_verbose opts
  debugmes verbose_mode "do_compile ... "
  -- TODO: regular way to add primitive names.
  let lv = (initialLevel $ Absyn.modid m){lv_dict=primNames}
  let st = st0{rn_modid = (lv_prefix lv), rn_lvs = (lv : rn_lvs st0)}
      ((bgs, as, dicts, ctab), st') = runState (renProg m cont) st

  when (opt_ddumpas opts) $ ddump_assump as
      
  let cmod = dsgModule (rn_modid st') bgs (as ++ rn_cms st) -- see memo#p-258
  let b = case cmod of
        Module _ [x] -> x
        _ -> error "Must not occur, cmod must be a Module."
      b' = tcBind b

  when (opt_ddumpcore opts) $ ddump_core b'
  
  let b'' = TR.trBind b'
      mname = case cmod of
        Module n _ -> n                      
  CodeGen.emitProgram b'' dest mname
  CodeGen.emitDicts dest dicts
  CodeGen.emitInsts dest dicts ctab
  debugmes verbose_mode "done.\n"

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnError) myParserInfo
  let verbose_mode = opt_verbose opts
  (cont, rnstate) <- case xnoImplicitPrelude opts of
    True -> return (initialTI, get_init_rnstate)
    False -> implicit_prelude (xlibPath opts) verbose_mode 
  let src = head $ inputFiles opts
      dest = destDir opts
  handle <- openFile src ReadMode
  s <- hGetContents handle
  let r = parse s
  case r of
    Left  mes -> putStrLn $ "Error: " ++ mes
    Right m -> do do_compile rnstate m dest cont opts

