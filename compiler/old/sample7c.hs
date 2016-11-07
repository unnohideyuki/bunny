f = putStrLn
e f = f "Hello!"

main :: IO ()
main = e f
