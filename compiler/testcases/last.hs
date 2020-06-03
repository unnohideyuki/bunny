last'        :: [a] -> a
last' [x]    =  [x]
last' (_:xs) =  last' xs

main = print $ last' "abcdef"
