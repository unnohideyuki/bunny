module CompilerOpts where

import Data.Monoid
import Options.Applicative

data Options = Options
               { destDir :: String
               , xlibPath :: String
               , xnoImplicitPrelude :: Bool
               , opt_verbose :: Bool
               , opt_ddumpas :: Bool
               , opt_ddumpcore :: Bool
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
           <*> sourceFilesP

myParserInfo :: ParserInfo Options
myParserInfo = info optionsP $ mconcat
               [ fullDesc
               , progDesc "Bunny, a Haskell compiler for Android."
               , header ""
               , footer ""
               ]
