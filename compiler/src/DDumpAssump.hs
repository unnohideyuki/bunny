module DDumpAssump where

import Typing

ddump_assump :: [Assump] -> IO ()
ddump_assump as = do
  putStrLn "Debugging dump of assumptions (results of the type inference)."
  ddump_assump' as
  where ddump_assump' [] = return ()

        ddump_assump' (x:xs) = do
          putStrLn $ passump x
          ddump_assump' xs

        passump (n :>: sc) = n ++ " :: " ++ show sc
