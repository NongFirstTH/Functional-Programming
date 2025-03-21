curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

curryThenUncurry :: ((a, b) -> c) -> (a, b) -> c
curryThenUncurry = uncurry' . curry'

uncurryThenCurry :: (a -> b -> c) -> a -> b -> c
uncurryThenCurry = curry' . uncurry'

facs = 1 : zipWith (*) [1..] facs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)