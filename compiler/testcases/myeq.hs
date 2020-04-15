LT `myeq` LT = True
EQ `myeq` EQ = True
GT `myeq` GT = True
_  `myeq` _  = False


main = do print $ LT `myeq` LT
          print $ EQ `myeq` GT



