elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = 
    if a == x then True
    else elem' a xs

elemFold :: (Foldable t, Eq a) => a -> t a -> Bool
elemFold a l = foldl (\acc x -> if a == x then True else acc) False l

-- rewrite partition
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x:xs)
  | p x       = (x:l, r)
  | otherwise = (l, x:r)
  where (l, r) = partition p xs

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p l = foldr (\x acc -> if p x then (x:(fst acc),snd acc) else ((fst acc),x:(snd acc))) ([], []) l
