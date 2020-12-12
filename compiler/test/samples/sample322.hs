-- problem_1 : https://wiki.haskell.org/Euler_problems/1_to_10#Problem_1
problem_1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
main = print problem_1
