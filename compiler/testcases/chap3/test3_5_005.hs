-- invalid
s = (let n = 10 in n +)
main = print $ s 5
