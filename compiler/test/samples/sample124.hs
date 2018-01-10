mappairs f [] ys = []
mappairs f (x:xs) [] = []
mappairs f (x:xs) (y:ys) = f x y : mappairs f xs ys

g x y = if x >= y then 't' else 'f'

main = putStrLn $ mappairs g "abcdefghi" "ddddd"

