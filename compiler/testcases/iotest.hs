f :: IO [Char]
f = return "foo"

main = do
  s <- f
  putStrLn s


