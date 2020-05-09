module PreDefined where
import           Symbol
import           Types
import           Typing

import           Data.Maybe (fromMaybe)

-- Primitive Constructors and Member Functions
unitCfun :: Assump
unitCfun  = "Prim.()" :>: Forall [] ([] :=> tUnit)

nilCfun :: Assump
nilCfun  = "Prelude.[]" :>: Forall [Star] ([] :=> TAp tList (TGen 0))

consCfun :: Assump
consCfun  = "Prelude.:" :>:
  Forall [Star]
  ([] :=> (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))

pairCfun :: Assump
pairCfun = "Prelude.(,)" :>:
    Forall [Star, Star]
    ([] :=> (TGen 0 `fn` TGen 1 `fn` pair (TGen 0) (TGen 1)))

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

primCharLeCfun :: Assump
primCharLeCfun = "Prim.charLe" :>: scCharcharbool

primCharEqCfun :: Assump
primCharEqCfun = "Prim.charEq" :>: scCharcharbool

scIntegerintegerbool :: Scheme
scIntegerintegerbool = Forall [] ([] :=> (tInteger `fn` tInteger `fn` tBool))

scIntegerintegerinteger :: Scheme
scIntegerintegerinteger =
  Forall [] ([] :=> (tInteger `fn` tInteger `fn` tInteger))

primIntegerLeCfun :: Assump
primIntegerLeCfun = "Prim.integerLe" :>: scIntegerintegerbool

primIntegerEqCfun :: Assump
primIntegerEqCfun = "Prim.integerEq" :>: scIntegerintegerbool

primIntegerAddCfun :: Assump
primIntegerAddCfun = "Prim.integerAdd" :>: scIntegerintegerinteger

primIntegerSubCfun :: Assump
primIntegerSubCfun = "Prim.integerSub" :>: scIntegerintegerinteger

primIntegerMulCfun :: Assump
primIntegerMulCfun = "Prim.integerMul" :>: scIntegerintegerinteger

scIntintbool :: Scheme
scIntintbool = Forall [] ([] :=> (tInt `fn` tInt `fn` tBool))

scIntintint :: Scheme
scIntintint =
  Forall [] ([] :=> (tInt `fn` tInt `fn` tInt))

primIntLeCfun :: Assump
primIntLeCfun = "Prim.intLe" :>: scIntintbool

primIntEqCfun :: Assump
primIntEqCfun = "Prim.intEq" :>: scIntintbool

primIntAddCfun :: Assump
primIntAddCfun = "Prim.intAdd" :>: scIntintint

primIntSubCfun :: Assump
primIntSubCfun = "Prim.intSub" :>: scIntintint

primIntMulCfun :: Assump
primIntMulCfun = "Prim.intMul" :>: scIntintint

primIntFromIntegerCfun :: Assump
primIntFromIntegerCfun = "Prim.intFromInteger" :>: Forall [] ([] :=> (tInteger `fn` tInt))

primIntegerFromIntCfun :: Assump
primIntegerFromIntCfun = "Prim.integerFromInt" :>: Forall [] ([] :=> (tInt `fn` tInteger))

showConNameCfun :: Assump
showConNameCfun  = "Prim.showConName" :>: Forall [Star] ([] :=> (TGen 0 `fn` tString))

integerShowCfun :: Assump
integerShowCfun  = "Prim.integerShow" :>: Forall [] ([] :=> (tInteger `fn` tString))

intShowCfun :: Assump
intShowCfun  = "Prim.intShow" :>: Forall [] ([] :=> (tInt `fn` tString))

integerQuotRem :: Assump
integerQuotRem  = "Prim.integerQuotRem" :>:
                  Forall [] ([] :=> (tInteger `fn` tInteger `fn` pair tInteger tInteger))

intQuotRem :: Assump
intQuotRem  = "Prim.intQuotRem" :>: Forall [] ([] :=> (tInt `fn` tInt `fn` pair tInt tInt))

overloadedCfun :: Assump
overloadedCfun = "#overloaded#" :>:
  Forall [Star, Star] ([] :=> (TGen 0 `fn` tString `fn` TGen 1))

primConsMems :: [Assump]
primConsMems  = [ unitCfun, nilCfun, consCfun
                , leMfun, gtMfun
                , primRetCfun, primBindCfun, primFailCfun
                , primCharLeCfun, primCharEqCfun
                , primIntegerLeCfun, primIntegerEqCfun
                , primIntegerAddCfun, primIntegerSubCfun
                , primIntegerMulCfun
                , primIntLeCfun, primIntEqCfun
                , primIntAddCfun, primIntSubCfun
                , primIntMulCfun
                , primIntFromIntegerCfun
                , primIntegerFromIntCfun
                , showConNameCfun
                , integerShowCfun, intShowCfun
                , integerQuotRem, intQuotRem
                , errorCfun
                , "Prim.putStrLn" :>:
                  Forall []
                   ([] :=> (TAp tList tChar `fn` TAp tIO tUnit))
                , overloadedCfun
                , "Main.main" :>: Forall [Star] ([] :=> TAp tIO (TGen 0))
                ]

primConsNames :: [(Id, Id)]
primConsNames  = [ ("()", "Prim.()")
                 , ("Prim.[]", "Prim.[]")
                 , ("[]", "Prelude.[]")
                 , (":", "Prelude.:")
                 , ("error", "Prim.error")
                 , ("Prim.retIO", "Prim.retIO")
                 , ("Prim.bindIO", "Prim.bindIO")
                 , ("Prim.failIO", "Prim.failIO")
                 , ("Prim.charLe", "Prim.charLe")
                 , ("Prim.charEq", "Prim.charEq")
                 , ("Prim.integerLe", "Prim.integerLe")
                 , ("Prim.integerEq", "Prim.integerEq")
                 , ("Prim.integerAdd", "Prim.integerAdd")
                 , ("Prim.integerSub", "Prim.integerSub")
                 , ("Prim.integerMul", "Prim.integerMul")
                 , ("Prim.intLe", "Prim.intLe")
                 , ("Prim.intEq", "Prim.intEq")
                 , ("Prim.intAdd", "Prim.intAdd")
                 , ("Prim.intSub", "Prim.intSub")
                 , ("Prim.intMul", "Prim.intMul")
                 , ("Prim.intFromInteger", "Prim.intFromInteger")
                 , ("Prim.integerFromInt", "Prim.integerFromInt")
                 , ("Prim.showConName", "Prim.showConName")
                 , ("Prim.integerShow", "Prim.integerShow")
                 , ("Prim.intShow", "Prim.intShow")
                 , ("Prim.integerQuotRem", "Prim.integerQuotRem")
                 , ("Prim.intQuotRem", "Prim.intQuotRem")
                 , ("#overloaded#", "#overloaded#")
                 ]

-- Primitive Names

primNames :: Table Id
primNames  = fromList (primConsNames ++
                       [ ("putStrLn", "Prim.putStrLn")
                       ])

-- arity and constructors for pattern-matching

data ConstructorInfo = ConstructorInfo { dArity  :: [(Id, Int)]
                                       , dConsts :: [(Id, [Assump])]
                                       }
                       deriving Show

emptyConstInfo :: ConstructorInfo
emptyConstInfo = ConstructorInfo [] []

concatConstInfo :: ConstructorInfo -> ConstructorInfo -> ConstructorInfo
concatConstInfo (ConstructorInfo da1 dc1)(ConstructorInfo da2 dc2) =
  ConstructorInfo (da1 ++ da2) (dc1 ++ dc2)

initialConsts :: ConstructorInfo
initialConsts = ConstructorInfo da dc
  where   da = [ ("Prim.()", 0)
               , ("Prelude.[]", 0)
               , ("Prelude.:", 2)
               , ("Prelude.(,)", 2)
               ]

          dc = [ ("Prim.()", [unitCfun])
               , ("Prelude.[]", [nilCfun, consCfun])
               , ("Prelude.:", [nilCfun, consCfun])
               , ("Prelude.(,)", [pairCfun])
               ]

arity :: ConstructorInfo -> Assump -> Int
arity ci (c :>: _) =
  fromMaybe (error $ "unknown arity: " ++ c) (lookup c (dArity ci))

constructors :: ConstructorInfo -> Assump -> [Assump]
constructors ci (c :>: _) =
    fromMaybe (error $ "unknown constructors: " ++ c) (lookup c (dConsts ci))

addConsts ::
  ConstructorInfo -> [(Id, Int)] -> [(Id, [Assump])] -> ConstructorInfo
addConsts ci@ConstructorInfo{dArity=da0, dConsts=dc0} da dc =
  ci{dArity = da0 ++ da, dConsts = dc0 ++ dc}

initialTypeConsts :: [(Id, Type)]
initialTypeConsts =
  [ ("()", tUnit)
  , ("Bool", tBool)
  , ("Char", tChar)
  , ("Int", tInt)
  , ("Integer", tInteger)
  , ("IO", tIO)
  , ("String", tString)
  ]
