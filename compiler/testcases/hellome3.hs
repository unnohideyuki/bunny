newtype CoolBool = CoolBool Bool

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

main = putStrLn $ helloMe undefined
