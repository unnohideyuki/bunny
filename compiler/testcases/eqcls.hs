data Hoge a = Fuga | Hoge a

meq :: (Eq t) => (Hoge t) -> (Hoge t) -> Bool
Fuga   `meq` Fuga   = True
Hoge x `meq` Hoge y = x == y
_      `meq` _      = False

main = print $ Fuga `meq` Hoge 5
