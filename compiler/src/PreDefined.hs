module PreDefined where
import Types
import Typing
import Symbol

-- preludeClasses

preludeClasses :: ClassEnv
preludeClasses  = case exampleInsts initialEnv of
  Just ce -> ce

-- Primitive Constructors and Member Functions

unitCfun :: Assump
unitCfun  = "Prim.()" :>: (Forall [] ([] :=> tUnit))

nilCfun :: Assump
nilCfun  = "Prim.[]" :>: (Forall [Star] ([] :=> (TAp tList (TGen 0))))

consCfun :: Assump
consCfun  = "Prim.:" :>: (Forall [Star]
                          ([] :=>
                           (TGen 0 `fn`
                            TAp tList (TGen 0) `fn`
                            TAp tList (TGen 0))))

falseCfun :: Assump
falseCfun  = "Prim.False" :>: (Forall [] ([] :=> tBool))
trueCfun :: Assump
trueCfun  = "Prim.True" :>: (Forall [] ([] :=> tBool))

cShow :: String
cShow  = "Main.Show"

cOrd :: String
cOrd  = "Main.Ord"

leMfun :: Assump
leMfun
  = "Prim.<=" :>: (Forall [Star]
                   ([IsIn cOrd (TGen 0)] :=>
                    (TGen 0 `fn` TGen 0 `fn` tBool)))

gtMfun :: Assump
gtMfun
  = "Prim.>" :>: (Forall [Star]
                  ([IsIn cOrd (TGen 0)] :=>
                   (TGen 0 `fn` TGen 0 `fn` tBool)))

errorCfun :: Assump
errorCfun
  = "Prim.error" :>: (Forall [Star]
                      ([] :=> (tString `fn` TGen 0)))

tIO :: Type
tIO = TCon (Tycon "Main.IO" (Kfun Star Star)) -- todo: Prim.IO

primRetCfun :: Assump
primRetCfun
  = "Prim.retIO" :>: (Forall [Star]
                          ([] :=> (TGen 0 `fn` TAp tIO (TGen 0))))
primBindCfun :: Assump
primBindCfun
  = "Prim.bindIO" :>:
    (Forall [Star, Star]
     ([] :=>
      (TAp tIO (TGen 0) `fn` (TGen 0 `fn` TAp tIO (TGen 1)) `fn` TAp tIO (TGen 1))))

primFailCfun :: Assump
primFailCfun
  = "Prim.failIO" :>: (Forall [Star]
                      ([] :=> (tString `fn` (TAp tIO (TGen 0)))))


sc_charcharbool = (Forall [] ([] :=> (tChar `fn` tChar `fn` tBool)))

primCharLtCfun :: Assump
primCharLtCfun = "Prim.charLt" :>: sc_charcharbool

primCharLeCfun :: Assump
primCharLeCfun = "Prim.charLe" :>: sc_charcharbool

primCharGeCfun :: Assump
primCharGeCfun = "Prim.charGe" :>: sc_charcharbool

primCharGtCfun :: Assump
primCharGtCfun = "Prim.charGt" :>: sc_charcharbool

primCharEqCfun :: Assump
primCharEqCfun = "Prim.charEq" :>: sc_charcharbool

sc_integerintegerbool
  = (Forall [] ([] :=> (tInteger `fn` tInteger `fn` tBool)))

primIntegerLtCfun :: Assump
primIntegerLtCfun = "Prim.integerLt" :>: sc_integerintegerbool

primIntegerLeCfun :: Assump
primIntegerLeCfun = "Prim.integerLe" :>: sc_integerintegerbool

primIntegerGeCfun :: Assump
primIntegerGeCfun = "Prim.integerGe" :>: sc_integerintegerbool

primIntegerGtCfun :: Assump
primIntegerGtCfun = "Prim.integerGt" :>: sc_integerintegerbool

primIntegerEqCfun :: Assump
primIntegerEqCfun = "Prim.integerEq" :>: sc_integerintegerbool

primCharShow :: Assump
primCharShow = "Prim.charShow" :>: (Forall [] ([] :=> (tChar `fn` tString)))

primIntegerShow :: Assump
primIntegerShow =
  "Prim.integerShow" :>: (Forall [] ([] :=> (tChar `fn` tString)))


overloadedCfun :: Assump
overloadedCfun = "#overloaded#" :>: (Forall [Star, Star]
                                     ([] :=> (TGen 0 `fn` tString `fn` TGen 1)))

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
                , primCharShow, primIntegerShow
                , errorCfun
                , "Prim.putStrLn" :>:
                  (Forall []
                   ([] :=> (TAp tList tChar `fn` TAp tIO tUnit)))
                , overloadedCfun
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
                 , ("Prim.charShow", "Prim.charShow")
                 , ("Prim.integerShow", "Prim.integerShow")
                 , ("#overloaded#", "#overloaded#")
                 ]

-- Primitive Names

primNames :: Table Id
primNames  = fromList (primConsNames ++
                       [ ("putStrLn", "Prim.putStrLn")
                       ])
             
