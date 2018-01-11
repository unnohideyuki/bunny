module DDumpCore where

import           Core
import           PPCore

ddumpCore :: Bind -> IO ()
ddumpCore b = do
  putStrLn "\n---- ddumpCore ----"
  putStrLn $ show $ ppBind b
