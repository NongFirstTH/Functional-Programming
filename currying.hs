join :: ([a], [a]) -> [a]
join ([], ys) = ys
join (x:xs, ys) = x : join (xs, ys)

join' :: [a] -> [a] -> [a]
join' [] ys = ys
join' (x:xs) ys = x : join' xs ys

func :: Num a => (a, a) -> a
func (x,y) = x + y

func2 :: Num a => a -> a -> a
func2 x y = x + y

currying :: ((a, b) -> c) -> a -> b -> c
currying = \f x y -> f(x,y)
-- usecase: currying func 1 3
-- make full application (func) -> partial application

uncurrying :: (a -> b -> c) -> (a, b) -> c
uncurrying = \g (x,y) -> g x y
-- usecase: uncurrying func2(1, 3) 
-- make partial application (func2) -> full application
