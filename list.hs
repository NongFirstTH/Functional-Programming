len :: Num b => [a] -> b
len [] = 0
len (_:xs) = 1 + len xs

join :: ([a], [a]) -> [a]
join ([], ys) = ys
join (x:xs, ys) = x : join (xs, ys)

zipper :: ([a], [b]) -> [(a,b)]
zipper (_, []) = []
zipper ([], _) = []
zipper (x:xs, y:ys) = (x,y):zipper(xs, ys)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = join (rev xs, [x])