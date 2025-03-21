maxList ::(Ord a, Num a) => [a] -> a
maxList [] = error "no max"
maxList [x] = x
-- maxList (x:xs) = 
    -- if x > (head xs) then x 
    -- else maxList xs
maxList (x:xs) = 
    let m = maximum xs in
    if m > x then m else x 

maxList' :: Ord a => [a] -> Maybe a
maxList' [] = Nothing
maxList' [x] = Just x
maxList' (x:xs) = 
    let m = maximum xs in
    Just (if m > x then m else x)

maxF :: (Foldable t, Ord a) => t a -> Maybe a
-- maxF l = foldl gt Nothing l
--     where
--         gt Nothing x = Just x
--         gt (Just m) x = Just $ max m x
maxF l = foldl (\m x -> Just $ maybe x (max x) m) Nothing l

len :: Num a1 => [a2] -> a1
len [] = 0
len (_:xs) = 1 + len xs

join :: [a] -> [a] -> [a]
join [] x = x 
join (x:xs) (ys) = x : join xs (ys)