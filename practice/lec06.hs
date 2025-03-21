filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x:xs)
    | pred x = x : filter' pred xs
    | otherwise = filter' pred xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' pred l = filter_Aux l []
    where
        filter_Aux [] res = res
        filter_Aux (x:xs) res 
            | pred x = filter_Aux xs (x : res)
            | otherwise = filter_Aux xs res

filter_concat :: ([a] -> Bool) -> [[a]] -> [a]
filter_concat _ [] = [] 
filter_concat pred (x:xs)
    | pred x = x ++ filter_concat pred xs
    | otherwise = filter_concat pred xs

filter_concat' :: ([a] -> Bool) -> [[a]] -> [a]
filter_concat' pred l = filterAux l []
    where
        filterAux [] res = res
        filterAux (x:xs) res
            | pred x = filterAux xs (res ++ x)
            | otherwise = filterAux xs res

fc :: ([a] -> Bool) -> [[a]] -> [a]
fc pred [] = []
fc pred l = (concat . filter pred) l

take_while :: (a -> Bool) -> [a] -> [a]
take_while _ [] = []
take_while pred (x:xs)
    | pred x = x : take_while pred xs
    | otherwise = []

-- (b -> Bool) -> ([a] -> b) -> a -> c
-- \l -> length l < 3
-- (<3).length