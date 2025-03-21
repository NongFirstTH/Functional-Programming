qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    qsort [lt | lt <- xs, lt < x] 
    ++ [x]
    ++ qsort [gt | gt <- xs, gt >= x]

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p [] = ([], [])
partition p (x:xs)
  | p x       = (x:l, r)
  | otherwise = (l, x:r)
  where (l, r) = partition p xs

fil :: (a -> Bool) -> [a] -> [a]
fil p = fst . partition p

qsort' [] = []
qsort' (hd:tl) = qsort l ++ [hd] ++ qsort r
  where (l, r) = partition (<hd) tl
