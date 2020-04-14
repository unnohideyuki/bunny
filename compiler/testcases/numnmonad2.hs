f g x y = g (x + y) >>= (\_ -> g (x * y))

main = f print 2 3
