module DDumpCore where

import Core

ddump_core :: Bind -> IO ()
ddump_core (Rec bs) = ddump_core' bs
  where ddump_core' [] = return ()

        ddump_core' ((v, e):bs') = do
          putStrLn "----------------------------------------"
          putStr $ show v
          putStrLn " = \n\n"
          putStrLn $ show e

          ddump_core' bs'
