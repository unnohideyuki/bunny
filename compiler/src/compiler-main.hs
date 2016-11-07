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

import Control.Monad.State.Strict (runState)

import Data.Monoid
import Options.Applicative

import System.IO

import Debug.Trace

data Options = Options
               { destDir :: String
               , xlibPath :: String
               , xnoImplicitPrelude :: Bool
               , inputFiles :: [FilePath]
               } deriving Show

destDirP :: Parser String
destDirP = strOption $ mconcat
           [ short 'd'
           , help "Destination directory"
           , metavar "directory"
           , value "."
           , showDefaultWith id
           ]

sourceFileP :: Parser FilePath
sourceFileP = strArgument $ mconcat
             [ help "source files to be compiled"
             , metavar "soucefiles"
             , action "file"
             ]

xlibPathP :: Parser String
xlibPathP = strOption $ mconcat
           [ long "xlibrary-path"
           , help "System library path"
           , metavar "directory"
           , value ""
           , showDefaultWith id
           ]

xNoImplicitPreludeP :: Parser Bool
xNoImplicitPreludeP = switch $ mconcat
                   [ long "xno-implicit-prelude"
                   , help "Suppress implicit loading of Prelude"
                   ]

sourceFilesP :: Parser [FilePath]
sourceFilesP = some sourceFileP

optionsP :: Parser Options
optionsP = (<*>) helper $
           Options
           <$> destDirP
           <*> xlibPathP
           <*> xNoImplicitPreludeP
           <*> sourceFilesP

myParserInfo :: ParserInfo Options
myParserInfo = info optionsP $ mconcat
               [ fullDesc
               , progDesc "Bunny, a Haskell compiler for Android."
               , header ""
               , footer ""
               ]

get_init_rnstate :: RnState
get_init_rnstate = RnState { rn_modid = ""
                           , rn_lvs = []
                           , rn_tenv = Symbol.empty
                           , rn_ifxenv = Symbol.empty
                           , rn_ce = preludeClasses
                           , rn_cms = primConsMems
                           , rn_tbs = []
                           , rn_tbstack = []
                           , rn_kdict = Symbol.empty
                           , rn_cdicts = []
                           }

implicit_prelude ::  String -> IO RnState
implicit_prelude prelude_dir = do
  trace "implicit_prelude ..." $ return ()
  let src = prelude_dir ++ "/Prelude.hs"
  h <- openFile src ReadMode
  s <- hGetContents h
  case parse s of
    Left mes -> error $ "Error: " ++ mes
    Right m -> trace "done." $ return $ do_implicit_prelude m
  where
    do_implicit_prelude m = 
      let  
        st0 = get_init_rnstate
        lv = (initialLevel $ Absyn.modid m){lv_dict=primNames}
        st = st0{rn_modid = (lv_prefix lv), rn_lvs = [lv]}
        ((_, as, _, _), st') = runState (renProg m) st
        as' = rn_cms st'
        st'' = st'{rn_cms = as ++ as'} -- see memo#p259
      in st''

do_compile :: RnState -> Absyn.Module -> String -> IO ()
do_compile st0 m dest = do
  trace "do_compile ..." $ return ()
  -- TODO: regular way to add primitive names.
  let lv = (initialLevel $ Absyn.modid m){lv_dict=primNames}
  let st = st0{rn_modid = (lv_prefix lv), rn_lvs = (lv : rn_lvs st0)}
      ((bgs, as, dicts, ctab), st') = runState (renProg m) st
  let cmod = dsgModule (rn_modid st') bgs (as ++ rn_cms st) -- see memo#p-258
  let b = case cmod of
        Module _ [b'] -> b'
      b' = tcBind b
      b'' = TR.trBind b'
      mname = case cmod of
        Module n _ -> n                      
  CodeGen.emitProgram b'' dest mname
  CodeGen.emitDicts dest dicts
  CodeGen.emitInsts dest dicts ctab
  trace "done." $ return ()

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnError) myParserInfo
  st <- case xnoImplicitPrelude opts of
    True -> return get_init_rnstate
    False -> implicit_prelude (xlibPath opts)
  let src = head $ inputFiles opts
      dest = destDir opts
  handle <- openFile src ReadMode
  s <- hGetContents handle
  let r = parse s
  case r of
    Left  mes -> putStrLn $ "Error: " ++ mes
    Right m -> do do_compile st m dest

