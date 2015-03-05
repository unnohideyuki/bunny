module PreDefined where
import Types
import Typing
import Symbol

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

cShow :: String
cShow  = "Prim.Show"

showMfun :: Assump
showMfun  = "Prim.show" :>: (Forall [Star]
                             ([IsIn cShow (TGen 0)] :=>
                              (TGen 0 `fn` tString)))

primConsMems :: [Assump]
primConsMems  = [unitCfun, nilCfun, consCfun, showMfun]

primConsNames :: [(Id, Id)]
primConsNames  = [ ("()", "Prim.()")
                 , ("[]", "Prim.[]")
                 , (":", "Prim.:")
                 , ("Show", "Prim.Show")
                 , ("show", "Prim.show")
                 ]

-- Primitive Names

primNames :: Table Id
primNames  = fromList primConsNames
