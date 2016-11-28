f s = -- [c | c <- s, c <= 'd']
  let
    ok c = if c <= 'd' then [c] else []
  in
   concatMap ok s

main = putStrLn $ f "abcdefgh"
