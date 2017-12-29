module DDumpCore where

import           Core

ddumpCore :: Bind -> IO ()
ddumpCore (Rec bs) = ddump_core' bs
  where ddump_core' [] = return ()

        ddump_core' ((v, e):bs') = do
          putStrLn "----------------------------------------"
          putStr $ show v
          putStrLn " = \n\n"
          print e

          ddump_core' bs'
