zipper :: ([a], [b]) -> [(a,b)]
zipper (_, []) = []
zipper ([], _) = []
zipper ((x:xs), (y:ys)) = (x, y):zipper (xs, ys)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- f :: t1 -> t2 -> [t1] -> [t2] -> [(t1, t2)]
-- f a b [as] [by] = []

zipper' :: [a] -> [b] -> [(a,b)]
zipper' _ [] = []
zipper' [] _ = []
zipper' (x:xs) (y:ys) = (x, y):zipper (xs, ys)

fac :: (Ord t, Num t) => t -> t
fac n
    | n < 0 = error "Negative number"
    | n == 0 = 1
    | otherwise = n*fac (n-1) 