s, s1:: String
s = map toEnum [0..0x21]
s1 = map toEnum [130]
main = do print s
          print s1
          print $ s1 ++ "abc"
          print $ s1 ++ "012"
