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

primConsMems :: [Assump]
primConsMems  = [ unitCfun, nilCfun, consCfun, showMfun
                , falseCfun, trueCfun
                ]

primConsNames :: [(Id, Id)]
primConsNames  = [ ("()", "Prim.()")
                 , ("[]", "Prim.[]")
                 , (":", "Prim.:")
                 , ("False", "Prim.False")
                 , ("True", "Prim.True")
                 , ("Show", "Prim.Show")
                 , ("show", "Prim.show")
                 ]

-- Primitive Names

primNames :: Table Id
primNames  = fromList primConsNames
