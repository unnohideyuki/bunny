module CompilerOpts where

import           Options.Applicative

verstr :: String
verstr = "0.0.1"

descstr :: String
descstr = "Bunny -- A Haskell compiler for Android."

data Options = Options
               { destDir            :: String
               , xlibPath           :: String
               , xnoImplicitPrelude :: Bool
               , optVerbose         :: Bool
               , optDdumpas         :: Bool
               , optDdumpcore       :: Bool
               , optDdumpcore0      :: Bool
               , inputFiles         :: [FilePath]
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
           , value "lib"
           , showDefaultWith id
           ]

xNoImplicitPreludeP :: Parser Bool
xNoImplicitPreludeP = switch $ mconcat
                   [ long "xno-implicit-prelude"
                   , help "Suppress implicit loading of Prelude"
                   ]

verboseP :: Parser Bool
verboseP = switch $ mconcat
           [ short 'v'
           , long "verbose"
           , help "Verbose mode"
           ]

ddumpasP :: Parser Bool
ddumpasP = switch $ mconcat
           [ long "ddump-assump"
           , help "Debugging dump of type inference"
           ]

ddumpcoreP :: Parser Bool
ddumpcoreP = switch $ mconcat
           [ long "ddump-core"
           , help "Debugging dump of Core"
           ]

ddumpcore0P :: Parser Bool
ddumpcore0P = switch $ mconcat
              [ long "ddump-core0"
              , help "Debugging dump of Core just after Semant"
              ]

sourceFilesP :: Parser [FilePath]
sourceFilesP = some sourceFileP

optionsP :: Parser Options
optionsP = (<*>) helper $
           Options
           <$> destDirP
           <*> xlibPathP
           <*> xNoImplicitPreludeP
           <*> verboseP
           <*> ddumpasP
           <*> ddumpcoreP
           <*> ddumpcore0P
           <*> sourceFilesP

myParserInfo :: ParserInfo Options
myParserInfo = info optionsP $ mconcat
               [ fullDesc
               , progDesc descstr
               , header ""
               , footer ""
               ]
