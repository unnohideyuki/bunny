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

initRnState :: RnState
initRnState =
  let
    ifxenv = insert "Prim.:" (Fixity RightAssoc 5) Symbol.empty
  in
   RnState { rnModid = ""
           , rnLvs = []
           , rnTenv = Symbol.empty
           , rnIfxenv = ifxenv
           , rnCe = initialEnv
           , rnCms = primConsMems
           , rnKdict = Symbol.empty
           , rnCdicts = []
           , rnConsts = emptyConstInfo
           , rnTConsts = initialTypeConsts
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
        st0 = initRnState
        lv = (initialLevel $ Absyn.modname m){lvDict=primNames}
        st = st0{rnModid = lvPrefix lv, rnLvs = [lv]}
        (cont, rnstate) = runState (semPrelude m) st
        as = rnCms rnstate
        as' = tiAs cont
      in (cont, rnstate{rnCms = as ++ as'})

doCompile :: RnState -> Absyn.Module -> String -> (Subst, Int, [Assump])
              -> Options -> IO ()
doCompile st0 m dest cont opts = do
  let verbose_mode = optVerbose opts
  debugmes verbose_mode "doCompile ... "
  -- TODO: regular way to add primitive names.
  let lv = (initialLevel $ Absyn.modname m){lvDict=primNames}
  let st = st0{rnModid = lvPrefix lv, rnLvs = lv : rnLvs st0}
      ((bgs, as, dicts, ctab, ce), st') = runState (semProgram m cont) st

  when (optDdumpas opts) $ ddumpAssump as

  let ci = rnConsts st'
      ci' = concatConstInfo initialConsts ci
  let cmod = dsgModule (rnModid st') bgs (as ++ rnCms st') ci' -- see memo#p-258
  let b = case cmod of
        Module _ [x] -> x
        _            -> error "Must not occur, cmod must be a Module."

  when (optDdumpcore0 opts) $ ddumpCore b

  let b' = tcBind b ce Nothing

  when (optDdumpcore opts) $ ddumpCore b'

  let b'' = TR.trBind b'
      mname = case cmod of
        Module n _ -> n
  CodeGen.emitProgram b'' dest mname ci
  CodeGen.emitDicts dest dicts
  CodeGen.emitInsts dest dicts ctab ce
  debugmes verbose_mode "done.\n"

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnError) myParserInfo
  let verbose_mode = optVerbose opts
  (cont, rne) <- if xnoImplicitPrelude opts
                     then return (initialTI, initRnState)
                     else implicitPrelude (xlibPath opts) verbose_mode
  let src = head $ inputFiles opts
      dest = destDir opts
  handle <- openFile src ReadMode
  s <- hGetContents handle
  let r = parse s
  case r of
    Left  mes -> putStrLn $ "Error: " ++ mes
    Right m   -> do
      when (optDdumpabsyn opts) $ print m
      doCompile rne m dest cont opts
