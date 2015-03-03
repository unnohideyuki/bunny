module StaticPrelude where
import Types
import Typing

unitCfun :: Assump
unitCfun  = "()" :>: (Forall [] ([] :=> tUnit))

nilCfun :: Assump
nilCfun  = "[]" :>: (Forall [Star] ([] :=> (TAp tList (TGen 0))))

consCfun :: Assump
consCfun  = ":" :>: (Forall [Star]
                     ([] :=>
                      (TGen 0 `fn`
                       TAp tList (TGen 0) `fn`
                       TAp tList (TGen 0))))
