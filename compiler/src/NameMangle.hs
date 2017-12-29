module NameMangle where

import           Symbol

{- modname: extracts Module name part from a varname -}
modname :: Id -> Id
modname = takeWhile (/= '.') -- todo: deeper module name

{- basename: extracts basename (= strip module name) from a varname -}
basename :: Id -> Id
basename s = drop (length (modname s) + 1) s

-- mangler function
mangle :: Id -> Id
mangle = escapeId

{- modnameM: mangling version of modname -}
modnameM :: Id -> Id
modnameM = mangle . modname

{- basenameM: mangling version of basename -}
basenameM :: Id -> Id
basenameM = mangle . basename

{- cls2dictNameM: generates dictionary name from a class name -}
cls2dictNameM :: Id -> Id
cls2dictNameM clsname = mangle $ "Dict$" ++ clsname
