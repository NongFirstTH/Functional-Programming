find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x:xs) =
    if p x
    then Just x
    else find p xs

elem' :: Eq t => t -> [t] -> Bool
elem' a [] = False
elem' a (x:xs) =
    if a == x then True
    else elem' a xs

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' a l = foldl (\acc x -> if a == x then True else acc) False l

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p [] = ([], [])
partition p (x:xs)
  | p x       = (x:l, r)
  | otherwise = (l, x:r)
  where (l, r) = partition p xs

partition' :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition' p l = foldr (\x acc -> if p x then (x: fst acc, snd acc) else (fst acc ,x: snd acc)) ([],[]) l