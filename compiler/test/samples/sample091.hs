main = f "Hello, function with free variable."
  where
    g = putStrLn
    f s = g s

