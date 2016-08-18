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
cShow  = "Prim.Show"

showMfun :: Assump
showMfun  = "Prim.show" :>: (Forall [Star]
                             ([IsIn cShow (TGen 0)] :=>
                              (TGen 0 `fn` tString)))

cOrd :: String
cOrd  = "Ord"

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

overloadedCfun :: Assump
overloadedCfun = "#overloaded#" :>: (Forall [Star, Star]
                                     ([] :=> (TGen 0 `fn` tString `fn` TGen 1)))

primConsMems :: [Assump]
primConsMems  = [ unitCfun, nilCfun, consCfun, showMfun
                , falseCfun, trueCfun
                , leMfun, gtMfun
                , primRetCfun, primBindCfun, primFailCfun
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
                 , ("Show", "Prim.Show")
                 , ("show", "Prim.show")
                 , ("error", "Prim.error")
                 , ("Prim.retIO", "Prim.retIO")
                 , ("Prim.bindIO", "Prim.bindIO")
                 , ("Prim.failIO", "Prim.failIO")
                 , ("PrimGt", "Prim.>")
                 , ("PrimLe", "Prim.<=")
                 , ("#overloaded#", "#overloaded#")
                 ]

-- Primitive Names

primNames :: Table Id
primNames  = fromList (primConsNames ++
                       [ ("putStrLn", "Prim.putStrLn")
                       ])
             
