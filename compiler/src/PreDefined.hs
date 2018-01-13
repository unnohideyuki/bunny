module PreDefined where
import           Symbol
import           Types
import           Typing

import           Data.Maybe (fromMaybe)

-- preludeClasses
preludeClasses :: ClassEnv
preludeClasses = fromMaybe (error "preludeClasses must not occur")
                 (exampleInsts initialEnv)

-- Primitive Constructors and Member Functions
unitCfun :: Assump
unitCfun  = "Prim.()" :>: Forall [] ([] :=> tUnit)

nilCfun :: Assump
nilCfun  = "Prim.[]" :>: Forall [Star] ([] :=> TAp tList (TGen 0))

consCfun :: Assump
consCfun  = "Prim.:" :>:
  Forall [Star]
  ([] :=> (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))

falseCfun :: Assump
falseCfun  = "Prim.False" :>: Forall [] ([] :=> tBool)

trueCfun :: Assump
trueCfun  = "Prim.True" :>: Forall [] ([] :=> tBool)

pairCfun :: Assump
pairCfun = "Prim.(,)" :>:
    Forall [Star, Star]
    ([] :=> (TGen 0 `fn` TGen 1 `fn` pair (TGen 0) (TGen 1)))

cShow :: String
cShow  = "Prim.Show"

cOrd :: String
cOrd  = "Prelude.Ord"

leMfun :: Assump
leMfun = "Prim.<=" :>:
  Forall [Star]
  ([IsIn cOrd (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))

gtMfun :: Assump
gtMfun = "Prim.>" :>:
  Forall [Star]
  ([IsIn cOrd (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))

errorCfun :: Assump
errorCfun = "Prim.error" :>: Forall [Star] ([] :=> (tString `fn` TGen 0))

tIO :: Type
tIO = TCon (Tycon "Prelude.IO" (Kfun Star Star)) -- TODO: Prim.IO

primRetCfun :: Assump
primRetCfun = "Prim.retIO" :>:
  Forall [Star] ([] :=> (TGen 0 `fn` TAp tIO (TGen 0)))

primBindCfun :: Assump
primBindCfun = "Prim.bindIO" :>:
  Forall [Star, Star]
  ([] :=>
   (TAp tIO (TGen 0) `fn` (TGen 0 `fn` TAp tIO (TGen 1)) `fn` TAp tIO (TGen 1)))

primFailCfun :: Assump
primFailCfun = "Prim.failIO" :>:
  Forall [Star] ([] :=> (tString `fn` TAp tIO (TGen 0)))

scCharcharbool :: Scheme
scCharcharbool = Forall [] ([] :=> (tChar `fn` tChar `fn` tBool))

primCharLtCfun :: Assump
primCharLtCfun = "Prim.charLt" :>: scCharcharbool

primCharLeCfun :: Assump
primCharLeCfun = "Prim.charLe" :>: scCharcharbool

primCharGeCfun :: Assump
primCharGeCfun = "Prim.charGe" :>: scCharcharbool

primCharGtCfun :: Assump
primCharGtCfun = "Prim.charGt" :>: scCharcharbool

primCharEqCfun :: Assump
primCharEqCfun = "Prim.charEq" :>: scCharcharbool

scIntegerintegerbool :: Scheme
scIntegerintegerbool = Forall [] ([] :=> (tInteger `fn` tInteger `fn` tBool))

scIntegerintegerinteger :: Scheme
scIntegerintegerinteger =
  Forall [] ([] :=> (tInteger `fn` tInteger `fn` tInteger))

primIntegerLtCfun :: Assump
primIntegerLtCfun = "Prim.integerLt" :>: scIntegerintegerbool

primIntegerLeCfun :: Assump
primIntegerLeCfun = "Prim.integerLe" :>: scIntegerintegerbool

primIntegerGeCfun :: Assump
primIntegerGeCfun = "Prim.integerGe" :>: scIntegerintegerbool

primIntegerGtCfun :: Assump
primIntegerGtCfun = "Prim.integerGt" :>: scIntegerintegerbool

primIntegerEqCfun :: Assump
primIntegerEqCfun = "Prim.integerEq" :>: scIntegerintegerbool

primIntegerAddCfun :: Assump
primIntegerAddCfun = "Prim.integerAdd" :>: scIntegerintegerinteger

primCharShow :: Assump
primCharShow = "Prim.charShow" :>: Forall [] ([] :=> (tChar `fn` tString))

primIntegerShow :: Assump
primIntegerShow =
  "Prim.integerShow" :>: Forall [] ([] :=> (tChar `fn` tString))

showCfun :: Assump
showCfun  = "Prim.show" :>: Forall [Star] ([] :=> (TGen 0 `fn` tString))

overloadedCfun :: Assump
overloadedCfun = "#overloaded#" :>:
  Forall [Star, Star] ([] :=> (TGen 0 `fn` tString `fn` TGen 1))

primConsMems :: [Assump]
primConsMems  = [ unitCfun, nilCfun, consCfun
                , falseCfun, trueCfun
                , leMfun, gtMfun
                , primRetCfun, primBindCfun, primFailCfun
                , primCharLtCfun, primCharLeCfun, primCharGeCfun, primCharGtCfun
                , primCharEqCfun
                , primIntegerLtCfun, primIntegerLeCfun
                , primIntegerGeCfun, primIntegerGtCfun
                , primIntegerEqCfun
                , primIntegerAddCfun
--                , primCharShow, primIntegerShow
                , showCfun
                , errorCfun
                , "Prim.putStrLn" :>:
                  Forall []
                   ([] :=> (TAp tList tChar `fn` TAp tIO tUnit))
                , overloadedCfun
                , "Main.main" :>: Forall [Star] ([] :=> TAp tIO (TGen 0))
                ]

primConsNames :: [(Id, Id)]
primConsNames  = [ ("()", "Prim.()")
                 , ("[]", "Prim.[]")
                 , (":", "Prim.:")
                 , ("False", "Prim.False")
                 , ("True", "Prim.True")
                 , ("error", "Prim.error")
                 , ("Prim.retIO", "Prim.retIO")
                 , ("Prim.bindIO", "Prim.bindIO")
                 , ("Prim.failIO", "Prim.failIO")
                 , ("Prim.charLt", "Prim.charLt")
                 , ("Prim.charLe", "Prim.charLe")
                 , ("Prim.charGe", "Prim.charGe")
                 , ("Prim.charGt", "Prim.charGt")
                 , ("Prim.charEq", "Prim.charEq")
                 , ("Prim.integerLt", "Prim.integerLt")
                 , ("Prim.integerLe", "Prim.integerLe")
                 , ("Prim.integerGe", "Prim.integerGe")
                 , ("Prim.integerGt", "Prim.integerGt")
                 , ("Prim.integerEq", "Prim.integerEq")
                 , ("Prim.integerAdd", "Prim.integerAdd")
--                 , ("Prim.charShow", "Prim.charShow")
--                 , ("Prim.integerShow", "Prim.integerShow")
                 , ("show", "Prim.show")
                 , ("#overloaded#", "#overloaded#")
                 ]

-- Primitive Names

primNames :: Table Id
primNames  = fromList (primConsNames ++
                       [ ("putStrLn", "Prim.putStrLn")
                       ])

-- arity and constructors for pattern-matching

arity :: Assump -> Int
arity (c :>: _) =
  case c of
    "Prim.()"    -> 0
    "Prim.[]"    -> 0
    "Prim.:"     -> 2
    "Prim.True"  -> 0
    "Prim.False" -> 0
    "Prim.(,)"   -> 2
    _            -> error $ "unknown arity: " ++ c

constructors :: Assump -> [Assump]
constructors (c :>: _) =
  case c of
    "Prim.()"    -> [unitCfun]
    "Prim.[]"    -> [nilCfun, consCfun]
    "Prim.:"     -> [nilCfun, consCfun]
    "Prim.True"  -> [falseCfun, trueCfun]
    "Prim.False" -> [falseCfun, trueCfun]
    "Prim.(,)"   -> [pairCfun]
    _            -> error $ "unknown constructors: " ++ c
