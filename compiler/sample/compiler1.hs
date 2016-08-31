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

sourceFilesP :: Parser [FilePath]
sourceFilesP = some sourceFileP

optionsP :: Parser Options
optionsP = (<*>) helper $
           Options <$> destDirP <*> sourceFilesP

myParserInfo :: ParserInfo Options
myParserInfo = info optionsP $ mconcat
               [ fullDesc
               , progDesc "Bunny, a Haskell compiler for Android."
               , header ""
               , footer ""
               ]

do_compile :: Absyn.Module -> String -> IO ()
do_compile m dest = do
  -- TODO: regular way to add primitive names.
  let lv = (initialLevel $ Absyn.modid m){lv_dict=primNames}
  let st = RnState (lv_prefix lv) [lv] Symbol.empty Symbol.empty preludeClasses primConsMems [] [] Symbol.empty []
      ((bgs, as, dicts, ctab), st') = runState (renProg m) st
  let cmod = dsgModule (rn_modid st') bgs (as ++ primConsMems)
  let b = case cmod of
        Module _ [b'] -> b'
      b' = tcBind b
      b'' = TR.trBind b'
      mname = case cmod of
        Module n _ -> n                      
  CodeGen.emitProgram b'' dest mname
  CodeGen.emitDicts dest dicts
  trace (show dicts) $ return ()
  trace (show ctab) $ return ()
  CodeGen.emitInsts dest dicts ctab

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnError) myParserInfo
  let src = head $ inputFiles opts
      dest = destDir opts
  handle <- openFile src ReadMode
  s <- hGetContents handle
  let r = parse s
  case r of
    Left  mes -> putStrLn $ "Error: " ++ mes
    Right m -> do do_compile m dest

