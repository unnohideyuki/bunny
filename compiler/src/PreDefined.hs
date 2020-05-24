module PreDefined where
import           Symbol
import           Types
import           Typing

import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)

fromAssumpList :: [Assump] -> Assumps
fromAssumpList xs = Map.fromList (map (\(i :>: sc) -> (i, sc)) xs)

toAssumpList :: Assumps -> [Assump]
toAssumpList as = map (\(i, sc) -> (i :>: sc)) (Map.toList as)

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

eFAILCfun :: Assump
eFAILCfun = "Prim.FAIL" :>: Forall [Star] ([] :=> (TGen 0))

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

scDoubledoublebool :: Scheme
scDoubledoublebool = Forall [] ([] :=> (tDouble `fn` tDouble `fn` tBool))

scDoubledoubledouble :: Scheme
scDoubledoubledouble =
  Forall [] ([] :=> (tDouble `fn` tDouble `fn` tDouble))

primDoubleLeCfun :: Assump
primDoubleLeCfun = "Prim.doubleLe" :>: scDoubledoublebool

primDoubleEqCfun :: Assump
primDoubleEqCfun = "Prim.doubleEq" :>: scDoubledoublebool

primDoubleAddCfun :: Assump
primDoubleAddCfun = "Prim.doubleAdd" :>: scDoubledoubledouble

primDoubleSubCfun :: Assump
primDoubleSubCfun = "Prim.doubleSub" :>: scDoubledoubledouble

primDoubleMulCfun :: Assump
primDoubleMulCfun = "Prim.doubleMul" :>: scDoubledoubledouble

primDoubleSignumCfun :: Assump
primDoubleSignumCfun = "Prim.doubleSignum" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleFromIntegerCfun :: Assump
primDoubleFromIntegerCfun = "Prim.doubleFromInteger" :>:
                            Forall [] ([] :=> (tInteger `fn` tDouble))

primDoubleShowCfun :: Assump
primDoubleShowCfun  = "Prim.doubleShow" :>: Forall [] ([] :=> (tDouble `fn` tString))

scFloatfloatbool :: Scheme
scFloatfloatbool = Forall [] ([] :=> (tFloat `fn` tFloat `fn` tBool))

scFloatfloatfloat :: Scheme
scFloatfloatfloat =
  Forall [] ([] :=> (tFloat `fn` tFloat `fn` tFloat))

primFloatLeCfun :: Assump
primFloatLeCfun = "Prim.floatLe" :>: scFloatfloatbool

primFloatEqCfun :: Assump
primFloatEqCfun = "Prim.floatEq" :>: scFloatfloatbool

primFloatAddCfun :: Assump
primFloatAddCfun = "Prim.floatAdd" :>: scFloatfloatfloat

primFloatSubCfun :: Assump
primFloatSubCfun = "Prim.floatSub" :>: scFloatfloatfloat

primFloatMulCfun :: Assump
primFloatMulCfun = "Prim.floatMul" :>: scFloatfloatfloat

primFloatSignumCfun :: Assump
primFloatSignumCfun = "Prim.floatSignum" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatFromIntegerCfun :: Assump
primFloatFromIntegerCfun = "Prim.floatFromInteger" :>:
                            Forall [] ([] :=> (tInteger `fn` tFloat))

primFloatShowCfun :: Assump
primFloatShowCfun  = "Prim.floatShow" :>: Forall [] ([] :=> (tFloat `fn` tString))

overloadedCfun :: Assump
overloadedCfun = "#overloaded#" :>:
  Forall [Star, Star] ([] :=> (TGen 0 `fn` tString `fn` TGen 1))

primConsMems :: [Assump]
primConsMems  = [ unitCfun, nilCfun, consCfun, pairCfun
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
                , primDoubleLeCfun, primDoubleEqCfun
                , primDoubleAddCfun, primDoubleSubCfun, primDoubleMulCfun
                , primDoubleSignumCfun, primDoubleFromIntegerCfun
                , primDoubleShowCfun
                , primFloatLeCfun, primFloatEqCfun
                , primFloatAddCfun, primFloatSubCfun, primFloatMulCfun
                , primFloatSignumCfun, primFloatFromIntegerCfun
                , primFloatShowCfun
                , errorCfun
                , eFAILCfun
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
                 , ("Prim.FAIL", "Prim.FAIL")
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
                 , ("Prim.doubleLe", "Prim.doubleLe")
                 , ("Prim.doubleEq", "Prim.doubleEq")
                 , ("Prim.doubleAdd", "Prim.doubleAdd")
                 , ("Prim.doubleSub", "Prim.doubleSub")
                 , ("Prim.doubleMul", "Prim.doubleMul")
                 , ("Prim.doubleSignum", "Prim.doubleSignum")
                 , ("Prim.doubleFromInteger", "Prim.doubleFromInteger")
                 , ("Prim.doubleShow", "Prim.doubleShow")
                 , ("Prim.floatLe", "Prim.floatLe")
                 , ("Prim.floatEq", "Prim.floatEq")
                 , ("Prim.floatAdd", "Prim.floatAdd")
                 , ("Prim.floatSub", "Prim.floatSub")
                 , ("Prim.floatMul", "Prim.floatMul")
                 , ("Prim.floatSignum", "Prim.floatSignum")
                 , ("Prim.floatFromInteger", "Prim.floatFromInteger")
                 , ("Prim.floatShow", "Prim.floatShow")
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
  , ("Double", tDouble)
  , ("Float", tFloat)
  ]
