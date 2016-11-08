module Main where

import Types
import Typing
import PreDefined

ce = preludeClasses

vs = []

ps = [IsIn "Prelude.Ord" (TVar (Tyvar ".v57" Star))]

main = do print ce
          print ps
          let vps = ambiguities ce vs ps
              tss = map (candidates ce) vps
          print vps
          print tss


