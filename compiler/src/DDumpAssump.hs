module DDumpAssump where

import           PpType
import           Typing

ddumpAssump :: [Assump] -> IO ()
ddumpAssump as = do
  putStrLn "Debugging dump of assumptions (results of the type inference)."
  ddump_assump' as
  where ddump_assump' [] = return ()

        ddump_assump' (x:xs) = do
          putStrLn $ passump x
          ddump_assump' xs

        passump (n :>: sc) = n ++ " :: " ++ pshow sc

pshow :: Scheme -> String
pshow (Forall ks (ps :=> t)) =
  "Forall " ++ show ks ++ " (" ++ show ps ++ " :=> " ++ ppType t ++ ")"
