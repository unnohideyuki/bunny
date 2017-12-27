module Main where

import qualified Absyn
import qualified CodeGen
import           Core
import           Desugar
import           DictPass                   (tcBind)
import           Parser
import           PreDefined
import           Semant
import           Symbol
import qualified TrSTG                      as TR
import           Typing                     (Assump, Subst, initialEnv,
                                             initialTI)

import           CompilerOpts
import           DDumpAssump
import           DDumpCore

import           Control.Monad
import           Control.Monad.State.Strict (runState)
import           Options.Applicative
import           System.IO

initRnstate :: RnState
initRnstate =
  let
    ifxenv = insert "Prim.:" (RightAssoc 5) Symbol.empty
  in
   RnState { rnstatModid = ""
           , rnstatLvs = []
           , rnstatTenv = Symbol.empty
           , rnstatIfxenv = ifxenv
           , rnstatCe = initialEnv -- preludeClasses
           , rnstatCms = primConsMems
           , rnstatTbs = []
           , rnstatTbsStack = []
           , rnstatKdict = Symbol.empty
           , rnstatCdicts = []
           }

tiAs :: (a, b, c) -> c
tiAs (_, _, as) = as

debugmes :: Bool -> String -> IO ()
debugmes verbose_mode message = when verbose_mode $ hPutStr stderr message

implicitPrelude :: String -> Bool -> IO ((Subst, Int, [Assump]), RnState)
implicitPrelude prelude_dir verbose_mode = do
  debugmes verbose_mode "implicitPrelude ... "
  let src = prelude_dir ++ "/Prelude.hs"
  h <- openFile src ReadMode
  s <- hGetContents h
  case parse s of
    Left mes -> error $ "Error: " ++ mes
    Right m -> do debugmes verbose_mode "done.\n"
                  return $ doImplicitPrelude m
  where
    doImplicitPrelude m =
      let
        st0 = initRnstate
        lv = (initialLevel $ Absyn.modid m){lvDict=primNames}
        st = st0{rnstatModid = lvPrefix lv, rnstatLvs = [lv]}
        (cont, rnstate) = runState (renPrelude m) st
        as = rnstatCms rnstate
        as' = tiAs cont
      in (cont, rnstate{rnstatCms = as ++ as'})

doCompile :: RnState -> Absyn.Module -> String -> (Subst, Int, [Assump])
              -> Options -> IO ()
doCompile st0 m dest cont opts = do
  let verbose_mode = opt_verbose opts
  debugmes verbose_mode "doCompile ... "
  -- TODO: regular way to add primitive names.
  let lv = (initialLevel $ Absyn.modid m){lvDict=primNames}
  let st = st0{rnstatModid = lvPrefix lv, rnstatLvs = lv : rnstatLvs st0}
      ((bgs, as, dicts, ctab), st') = runState (renProg m cont) st

  when (opt_ddumpas opts) $ ddump_assump as

  let cmod = dsgModule (rnstatModid st') bgs (as ++ rnstatCms st) -- see memo#p-258
  let b = case cmod of
        Module _ [x] -> x
        _            -> error "Must not occur, cmod must be a Module."
      b' = tcBind b

  when (opt_ddumpcore opts) $ ddump_core b
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
  (cont, rnstate) <- if xnoImplicitPrelude opts
                     then return (initialTI, initRnstate)
                     else implicitPrelude (xlibPath opts) verbose_mode
  let src = head $ inputFiles opts
      dest = destDir opts
  handle <- openFile src ReadMode
  s <- hGetContents handle
  let r = parse s
  case r of
    Left  mes -> putStrLn $ "Error: " ++ mes
    Right m   -> doCompile rnstate m dest cont opts

