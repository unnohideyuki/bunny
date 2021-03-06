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
import           Typing                     (Assumps, Subst, initialEnv,
                                             initialTI, tiProgram)

import           CompilerOpts
import           DDumpAssump
import           DDumpCore
import           PPTyping

import           Control.Monad
import           Control.Monad.State.Strict (runState)
import qualified Data.Map.Strict            as Map
import           Debug.Trace
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
           , rnCms = fromAssumpList primConsMems
           , rnKdict = Symbol.empty
           , rnCdicts = []
           , rnConsts = emptyConstInfo
           , rnTConsts = initialTypeConsts
           , rnNum = 0
           , rnSyn = []
           , rnIContext = []
           }

tiAs :: (a, b, c) -> c
tiAs (_, _, as) = as

debugmes :: Bool -> String -> IO ()
debugmes verbose_mode message = when verbose_mode $ hPutStr stderr message

implicitPrelude :: String -> Bool -> IO (((Subst, Int, Assumps), [DictDef]), RnState)
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
        ((cont, dicts), rnstate) = runState (semPrelude m) st
        as = rnCms rnstate
        as' = tiAs cont
      in ((cont, dicts), rnstate{rnCms = Map.union as as'})

doCompile :: RnState -> Absyn.Module -> String -> (Subst, Int, Assumps)
             -> [DictDef] -> Options -> IO ()
doCompile st0 m dest cont idicts opts = do
  let verbose_mode = optVerbose opts
  debugmes verbose_mode "doCompile ... "
  -- TODO: regular way to add primitive names.
  let lv = (initialLevel $ Absyn.modname m){lvDict=primNames}
  let st = st0{rnModid = lvPrefix lv, rnLvs = lv : rnLvs st0}
      ((bgs_ti, bgs, as2, dicts, ctab), st') = runState (renProg m) st

  when (optDdumpren opts) $ ddumpRen bgs

  let ce = rnCe st'
      as' = tiProgram ce (Map.union (rnCms st') as2) bgs_ti cont
      as = Map.union as' as2

  when (optDdumpas opts) $ ddumpAssump (toAssumpList as)

  let ci = rnConsts st'
      ci' = concatConstInfo initialConsts ci
  let cmod = dsgModule (rnModid st') bgs (Map.union as (rnCms st')) ci' -- see memo#p-258
  let b = case cmod of
        Module _ [x] -> x
        _            -> error "Must not occur, cmod must be a Module."

  when (optDdumpcore0 opts) $ ddumpCore b

  let iContext = rnIContext st'
      (b', _) = tcBind b ce Nothing iContext

  when (optDdumpcore opts) $ ddumpCore b'

  let b'' = TR.trBind b'
      mname = case cmod of
        Module n _ -> n
      pkgname = optPackageString opts
  CodeGen.emitProgram b'' dest mname ci pkgname
  CodeGen.emitDicts dest dicts pkgname
  CodeGen.emitInsts dest (dicts++idicts) ctab ce pkgname
  debugmes verbose_mode "done.\n"

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnError) myParserInfo
  let verbose_mode = optVerbose opts
  ((cont, dicts), rne) <- if xnoImplicitPrelude opts
                          then return ((initialTI, []), initRnState)
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
      doCompile rne m dest cont dicts opts
