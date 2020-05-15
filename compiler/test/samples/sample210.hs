a `myeq` b = case (a, b) of
  (LT, LT) -> True
  (EQ, EQ) -> True
  (GT, GT) -> True
  (_ , _ ) -> False

main = do print $ LT `myeq` LT
          print $ EQ `myeq` GT



