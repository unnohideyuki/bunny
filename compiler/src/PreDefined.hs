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
unitCfun  = "Prelude.()" :>: Forall [] ([] :=> tUnit)

nilCfun :: Assump
nilCfun  = "Prelude.[]" :>: Forall [Star] ([] :=> TAp tList (TGen 0))

consCfun :: Assump
consCfun  = "Prelude.:" :>:
  Forall [Star]
  ([] :=> (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))

pairCfun :: Assump
pairCfun =  "Prelude.(,)" :>:
  Forall [Star, Star]
  ([] :=> (TGen 0 `fn` TGen 1 `fn` pair (TGen 0) (TGen 1)))

tripleCfun :: Assump
tripleCfun =  "Prelude.(,,)" :>:
  Forall [Star, Star, Star]
  ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` triple (TGen 0) (TGen 1) (TGen 2)))

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

primIntToCharCfun :: Assump
primIntToCharCfun = "Prim.intToChar" :>: Forall [] ([] :=> (tInt `fn` tChar))

primCharToIntCfun :: Assump
primCharToIntCfun = "Prim.charToInt" :>: Forall [] ([] :=> (tChar `fn` tInt))

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

integerShowCfun :: Assump
integerShowCfun  = "Prim.integerShow" :>: Forall [] ([] :=> (tInteger `fn` tString))

intShowCfun :: Assump
intShowCfun  = "Prim.intShow" :>: Forall [] ([] :=> (tInt `fn` tString))

integerQuotRem :: Assump
integerQuotRem  = "Prim.integerQuotRem" :>:
                  Forall [] ([] :=> (tInteger `fn` tInteger `fn` pair tInteger tInteger))

intQuotRem :: Assump
intQuotRem  = "Prim.intQuotRem" :>: Forall [] ([] :=> (tInt `fn` tInt `fn` pair tInt tInt))

intMaxBoundCfun :: Assump
intMaxBoundCfun  = "Prim.intMaxBound" :>: Forall [] ([] :=> tInt)

intMinBoundCfun :: Assump
intMinBoundCfun  = "Prim.intMinBound" :>: Forall [] ([] :=> tInt)

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

primDoubleDivCfun :: Assump
primDoubleDivCfun = "Prim.doubleDiv" :>: scDoubledoubledouble

primDoubleSignumCfun :: Assump
primDoubleSignumCfun = "Prim.doubleSignum" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleFromIntegerCfun :: Assump
primDoubleFromIntegerCfun = "Prim.doubleFromInteger" :>:
                            Forall [] ([] :=> (tInteger `fn` tDouble))

primDoubleToRationalCfun :: Assump
primDoubleToRationalCfun = "Prim.doubleToRational" :>:
                            Forall [] ([] :=> (tDouble `fn` tRational))

primDoubleFromRationalCfun :: Assump
primDoubleFromRationalCfun =  "Prim.doubleFromRational" :>:
                              Forall [] ([] :=> (tRational `fn` tDouble))

primDoubleExpCfun :: Assump
primDoubleExpCfun =  "Prim.doubleExp" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleLogCfun :: Assump
primDoubleLogCfun =  "Prim.doubleLog" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoublePowCfun :: Assump
primDoublePowCfun =  "Prim.doublePow" :>: Forall [] ([] :=> (tDouble `fn` tDouble `fn` tDouble))

primDoubleSinCfun :: Assump
primDoubleSinCfun =  "Prim.doubleSin" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleCosCfun :: Assump
primDoubleCosCfun =  "Prim.doubleCos" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleTanCfun :: Assump
primDoubleTanCfun =  "Prim.doubleTan" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleSinhCfun :: Assump
primDoubleSinhCfun =  "Prim.doubleSinh" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleCoshCfun :: Assump
primDoubleCoshCfun =  "Prim.doubleCosh" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleTanhCfun :: Assump
primDoubleTanhCfun =  "Prim.doubleTanh" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleAsinCfun :: Assump
primDoubleAsinCfun =  "Prim.doubleAsin" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleAcosCfun :: Assump
primDoubleAcosCfun =  "Prim.doubleAcos" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleAtanCfun :: Assump
primDoubleAtanCfun =  "Prim.doubleAtan" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleAsinhCfun :: Assump
primDoubleAsinhCfun =  "Prim.doubleAsinh" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleAcoshCfun :: Assump
primDoubleAcoshCfun =  "Prim.doubleAcosh" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleAtanhCfun :: Assump
primDoubleAtanhCfun =  "Prim.doubleAtanh" :>: Forall [] ([] :=> (tDouble `fn` tDouble))

primDoubleDecodeFloatCfun :: Assump
primDoubleDecodeFloatCfun =  "Prim.doubleDecodeFloat" :>: Forall [] ([] :=> (tDouble `fn` pair tInteger tInt))

primDoubleIsNaNCfun :: Assump
primDoubleIsNaNCfun =  "Prim.doubleIsNaN" :>: Forall [] ([] :=> (tDouble `fn` tBool))

primDoubleIsInfiniteCfun :: Assump
primDoubleIsInfiniteCfun =  "Prim.doubleIsInfinite" :>: Forall [] ([] :=> (tDouble `fn` tBool))

primDoubleIsDenormalizedCfun :: Assump
primDoubleIsDenormalizedCfun =  "Prim.doubleIsDenormalized" :>: Forall [] ([] :=> (tDouble `fn` tBool))

primDoubleIsNegativeZeroCfun :: Assump
primDoubleIsNegativeZeroCfun =  "Prim.doubleIsNegativeZero" :>: Forall [] ([] :=> (tDouble `fn` tBool))

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

primFloatDivCfun :: Assump
primFloatDivCfun = "Prim.floatDiv" :>: scFloatfloatfloat

primFloatSignumCfun :: Assump
primFloatSignumCfun = "Prim.floatSignum" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatFromIntegerCfun :: Assump
primFloatFromIntegerCfun = "Prim.floatFromInteger" :>:
                            Forall [] ([] :=> (tInteger `fn` tFloat))

primFloatToRationalCfun :: Assump
primFloatToRationalCfun = "Prim.floatToRational" :>:
                          Forall [] ([] :=> (tFloat `fn` tRational))

primFloatFromRationalCfun :: Assump
primFloatFromRationalCfun =  "Prim.floatFromRational" :>:
                             Forall [] ([] :=> (tRational `fn` tFloat))

primFloatExpCfun :: Assump
primFloatExpCfun =  "Prim.floatExp" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatLogCfun :: Assump
primFloatLogCfun =  "Prim.floatLog" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatPowCfun :: Assump
primFloatPowCfun =  "Prim.floatPow" :>: Forall [] ([] :=> (tFloat `fn` tFloat `fn` tFloat))

primFloatSinCfun :: Assump
primFloatSinCfun =  "Prim.floatSin" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatCosCfun :: Assump
primFloatCosCfun =  "Prim.floatCos" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatTanCfun :: Assump
primFloatTanCfun =  "Prim.floatTan" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatSinhCfun :: Assump
primFloatSinhCfun =  "Prim.floatSinh" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatCoshCfun :: Assump
primFloatCoshCfun =  "Prim.floatCosh" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatTanhCfun :: Assump
primFloatTanhCfun =  "Prim.floatTanh" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatAsinCfun :: Assump
primFloatAsinCfun =  "Prim.floatAsin" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatAcosCfun :: Assump
primFloatAcosCfun =  "Prim.floatAcos" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatAtanCfun :: Assump
primFloatAtanCfun =  "Prim.floatAtan" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatAsinhCfun :: Assump
primFloatAsinhCfun =  "Prim.floatAsinh" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatAcoshCfun :: Assump
primFloatAcoshCfun =  "Prim.floatAcosh" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatAtanhCfun :: Assump
primFloatAtanhCfun =  "Prim.floatAtanh" :>: Forall [] ([] :=> (tFloat `fn` tFloat))

primFloatDecodeFloatCfun :: Assump
primFloatDecodeFloatCfun =  "Prim.floatDecodeFloat" :>: Forall [] ([] :=> (tFloat `fn` pair tInteger tInt))

primFloatIsNaNCfun :: Assump
primFloatIsNaNCfun =  "Prim.floatIsNaN" :>: Forall [] ([] :=> (tFloat `fn` tBool))

primFloatIsInfiniteCfun :: Assump
primFloatIsInfiniteCfun =  "Prim.floatIsInfinite" :>: Forall [] ([] :=> (tFloat `fn` tBool))

primFloatIsDenormalizedCfun :: Assump
primFloatIsDenormalizedCfun =  "Prim.floatIsDenormalized" :>: Forall [] ([] :=> (tFloat `fn` tBool))

primFloatIsNegativeZeroCfun :: Assump
primFloatIsNegativeZeroCfun =  "Prim.floatIsNegativeZero" :>: Forall [] ([] :=> (tFloat `fn` tBool))

primFloatShowCfun :: Assump
primFloatShowCfun  = "Prim.floatShow" :>: Forall [] ([] :=> (tFloat `fn` tString))

primPutCharCfun :: Assump
primPutCharCfun =
  "Prim.putChar" :>: Forall [] ([] :=> (tChar `fn` TAp tIO tUnit))

primPutStrLnCfun :: Assump
primPutStrLnCfun =
  "Prim.putStrLn" :>: Forall [] ([] :=> (TAp tList tChar `fn` TAp tIO tUnit))

primGetCharCfun :: Assump
primGetCharCfun = "Prim.getChar" :>: Forall [] ([] :=> (TAp tIO tChar))

primSeqCfun :: Assump
primSeqCfun =
  "Prim.seq" :>: Forall [Star, Star] ([] :=> ((TGen 0) `fn` ((TGen 1) `fn` (TGen 1))))

overloadedCfun :: Assump
overloadedCfun = "#overloaded#" :>:
  Forall [Star, Star] ([] :=> (TGen 0 `fn` tString `fn` TGen 1))

primConsMems :: [Assump]
primConsMems  = [ unitCfun, nilCfun, consCfun, pairCfun, tripleCfun
                , leMfun, gtMfun
                , primRetCfun, primBindCfun, primFailCfun
                , primCharLeCfun, primCharEqCfun
                , primIntToCharCfun, primCharToIntCfun
                , primIntegerLeCfun, primIntegerEqCfun
                , primIntegerAddCfun, primIntegerSubCfun
                , primIntegerMulCfun
                , primIntLeCfun, primIntEqCfun
                , primIntAddCfun, primIntSubCfun
                , primIntMulCfun
                , primIntFromIntegerCfun
                , primIntegerFromIntCfun
                , integerShowCfun, intShowCfun
                , integerQuotRem, intQuotRem
                , intMaxBoundCfun, intMinBoundCfun
                , primDoubleLeCfun, primDoubleEqCfun
                , primDoubleAddCfun, primDoubleSubCfun, primDoubleMulCfun
                , primDoubleDivCfun
                , primDoubleSignumCfun, primDoubleFromIntegerCfun
                , primDoubleToRationalCfun, primDoubleFromRationalCfun
                , primDoubleExpCfun, primDoubleLogCfun, primDoublePowCfun
                , primDoubleSinCfun, primDoubleCosCfun, primDoubleTanCfun
                , primDoubleSinhCfun, primDoubleCoshCfun, primDoubleTanhCfun
                , primDoubleAsinCfun, primDoubleAcosCfun, primDoubleAtanCfun
                , primDoubleAsinhCfun, primDoubleAcoshCfun, primDoubleAtanhCfun
                , primDoubleDecodeFloatCfun
                , primDoubleIsNaNCfun, primDoubleIsInfiniteCfun
                , primDoubleIsDenormalizedCfun, primDoubleIsNegativeZeroCfun
                , primDoubleShowCfun
                , primFloatLeCfun, primFloatEqCfun
                , primFloatAddCfun, primFloatSubCfun, primFloatMulCfun
                , primFloatDivCfun
                , primFloatSignumCfun, primFloatFromIntegerCfun
                , primFloatToRationalCfun, primFloatFromRationalCfun
                , primFloatExpCfun, primFloatLogCfun, primFloatPowCfun
                , primFloatSinCfun, primFloatCosCfun, primFloatTanCfun
                , primFloatSinhCfun, primFloatCoshCfun, primFloatTanhCfun
                , primFloatAsinCfun, primFloatAcosCfun, primFloatAtanCfun
                , primFloatAsinhCfun, primFloatAcoshCfun, primFloatAtanhCfun
                , primFloatDecodeFloatCfun
                , primFloatIsNaNCfun, primFloatIsInfiniteCfun
                , primFloatIsDenormalizedCfun, primFloatIsNegativeZeroCfun
                , primFloatShowCfun
                , errorCfun
                , eFAILCfun
                , primSeqCfun
                , primPutCharCfun
                , primPutStrLnCfun
                , primGetCharCfun
                , overloadedCfun
                ]

primConsNames :: [(Id, Id)]
primConsNames  = [ ("()", "Prelude.()")
                 , ("Prim.[]", "Prim.[]")
                 , ("[]", "Prelude.[]")
                 , (":", "Prelude.:")
                 , ("#overloaded#", "#overloaded#")
                 , ("Prim.FAIL", "Prim.FAIL")
                 , ("(,,)", "Prelude.(,,)")
                 ]

-- Primitive Names

primNames :: Table Id
primNames  = fromList (primConsNames ++
                       [ ("Prim.charLe", "Prim.charLe")
                       , ("Prim.charEq", "Prim.charEq")
                       , ("Prim.intToChar", "Prim.intToChar")
                       , ("Prim.charToInt", "Prim.charToInt")
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
                       , ("Prim.integerShow", "Prim.integerShow")
                       , ("Prim.intShow", "Prim.intShow")
                       , ("Prim.integerQuotRem", "Prim.integerQuotRem")
                       , ("Prim.intQuotRem", "Prim.intQuotRem")
                       , ("Prim.intMaxBound", "Prim.intMaxBound")
                       , ("Prim.intMinBound", "Prim.intMinBound")
                       , ("Prim.doubleLe", "Prim.doubleLe")
                       , ("Prim.doubleEq", "Prim.doubleEq")
                       , ("Prim.doubleAdd", "Prim.doubleAdd")
                       , ("Prim.doubleSub", "Prim.doubleSub")
                       , ("Prim.doubleMul", "Prim.doubleMul")
                       , ("Prim.doubleDiv", "Prim.doubleDiv")
                       , ("Prim.doubleSignum", "Prim.doubleSignum")
                       , ("Prim.doubleFromInteger", "Prim.doubleFromInteger")
                       , ("Prim.doubleToRational", "Prim.doubleToRational")
                       , ("Prim.doubleFromRational", "Prim.doubleFromRational")
                       , ("Prim.doubleExp", "Prim.doubleExp")
                       , ("Prim.doubleLog", "Prim.doubleLog")
                       , ("Prim.doublePow", "Prim.doublePow")
                       , ("Prim.doubleSin", "Prim.doubleSin")
                       , ("Prim.doubleCos", "Prim.doubleCos")
                       , ("Prim.doubleTan", "Prim.doubleTan")
                       , ("Prim.doubleSinh", "Prim.doubleSinh")
                       , ("Prim.doubleCosh", "Prim.doubleCosh")
                       , ("Prim.doubleTanh", "Prim.doubleTanh")
                       , ("Prim.doubleAsin", "Prim.doubleAsin")
                       , ("Prim.doubleAcos", "Prim.doubleAcos")
                       , ("Prim.doubleAtan", "Prim.doubleAtan")
                       , ("Prim.doubleAsinh", "Prim.doubleAsinh")
                       , ("Prim.doubleAcosh", "Prim.doubleAcosh")
                       , ("Prim.doubleAtanh", "Prim.doubleAtanh")
                       , ("Prim.doubleDecodeFloat", "Prim.doubleDecodeFloat")
                       , ("Prim.doubleIsNaN", "Prim.doubleIsNaN")
                       , ("Prim.doubleIsInfinite", "Prim.doubleIsInfinite")
                       , ("Prim.doubleIsDenormalized", "Prim.doubleIsDenormalized")
                       , ("Prim.doubleIsNegativeZero", "Prim.doubleIsNegativeZero")
                       , ("Prim.doubleShow", "Prim.doubleShow")
                       , ("Prim.floatLe", "Prim.floatLe")
                       , ("Prim.floatEq", "Prim.floatEq")
                       , ("Prim.floatAdd", "Prim.floatAdd")
                       , ("Prim.floatSub", "Prim.floatSub")
                       , ("Prim.floatMul", "Prim.floatMul")
                       , ("Prim.floatDiv", "Prim.floatDiv")
                       , ("Prim.floatSignum", "Prim.floatSignum")
                       , ("Prim.floatFromInteger", "Prim.floatFromInteger")
                       , ("Prim.floatToRational", "Prim.floatToRational")
                       , ("Prim.floatFromRational", "Prim.floatFromRational")
                       , ("Prim.floatExp", "Prim.floatExp")
                       , ("Prim.floatLog", "Prim.floatLog")
                       , ("Prim.floatPow", "Prim.floatPow")
                       , ("Prim.floatSin", "Prim.floatSin")
                       , ("Prim.floatCos", "Prim.floatCos")
                       , ("Prim.floatTan", "Prim.floatTan")
                       , ("Prim.floatSinh", "Prim.floatSinh")
                       , ("Prim.floatCosh", "Prim.floatCosh")
                       , ("Prim.floatTanh", "Prim.floatTanh")
                       , ("Prim.floatAsin", "Prim.floatAsin")
                       , ("Prim.floatAcos", "Prim.floatAcos")
                       , ("Prim.floatAtan", "Prim.floatAtan")
                       , ("Prim.floatAsinh", "Prim.floatAsinh")
                       , ("Prim.floatAcosh", "Prim.floatAcosh")
                       , ("Prim.floatAtanh", "Prim.floatAtanh")
                       , ("Prim.floatDecodeFloat", "Prim.floatDecodeFloat")
                       , ("Prim.floatIsNaN", "Prim.floatIsNaN")
                       , ("Prim.floatIsInfinite", "Prim.floatIsInfinite")
                       , ("Prim.floatIsDenormalized", "Prim.floatIsDenormalized")
                       , ("Prim.floatIsNegativeZero", "Prim.floatIsNegativeZero")
                       , ("Prim.floatShow", "Prim.floatShow")
                       , ("Prim.error", "Prim.error")
                       , ("Prim.retIO", "Prim.retIO")
                       , ("Prim.bindIO", "Prim.bindIO")
                       , ("Prim.failIO", "Prim.failIO")
                       , ("Prim.seq", "Prim.seq")
                       , ("Prim.putChar", "Prim.putChar")
                       , ("Prim.putStrLn", "Prim.putStrLn")
                       , ("Prim.getChar", "Prim.getChar")
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
  where   da = [ ("Prelude.()", 0)
               , ("Prelude.[]", 0)
               , ("Prelude.:", 2)
               , ("Prelude.(,)", 2)
               , ("Prelude.(,,)", 3)
               ]

          dc = [ ("Prelude.()", [unitCfun])
               , ("Prelude.[]", [nilCfun, consCfun])
               , ("Prelude.:", [nilCfun, consCfun])
               , ("Prelude.(,)", [pairCfun])
               , ("Prelude.(,,)", [tripleCfun])
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
  , ("Double", tDouble)
  , ("Float", tFloat)
  , ("[]", tList)
  ]
