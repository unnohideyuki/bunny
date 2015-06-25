module PreDefined where
import Types
import Typing
import Symbol

-- preludeClasses

preludeClasses :: ClassEnv
preludeClasses  = initialEnv

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
  = "Main.error" :>: (Forall [Star]
                      ([] :=> (tString `fn` TGen 0)))

tIO :: Type
tIO = TCon (Tycon "Main.IO" (Kfun Star Star)) -- todo: Prim.IO

primRetCfun :: Assump
primRetCfun
  = "Prim.primRetIO" :>: (Forall [Star]
                          ([] :=> (TGen 0 `fn` TAp tIO (TGen 0))))
primBindCfun :: Assump
primBindCfun
  = "Prim.primBindIO" :>:
    (Forall [Star, Star]
     ([] :=>
      (TAp tIO (TGen 0) `fn` (TGen 0 `fn` TAp tIO (TGen 1)) `fn` TAp tIO (TGen 1))))

primFailCfun :: Assump
primFailCfun
  = "Prim.primFailIO" :>: (Forall [Star]
                      ([] :=> (tString `fn` (TAp tIO (TGen 0)))))

primConsMems :: [Assump]
primConsMems  = [ unitCfun, nilCfun, consCfun, showMfun
                , falseCfun, trueCfun
                , leMfun, gtMfun
                , primRetCfun, primBindCfun, primFailCfun
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
                 , ("primRetIO", "Prim.primRetIO")
                 , ("primBindIO", "Prim.primBindIO")
                 , ("primFailIO", "Prim.primFailIO")
                 , ("PrimGt", "Prim.>")
                 , ("PrimLe", "Prim.<=")
                 ]

-- Primitive Names

primNames :: Table Id
primNames  = fromList (primConsNames ++
                       [ ("putStrLn", "Prim.putStrLn")
                       ])

