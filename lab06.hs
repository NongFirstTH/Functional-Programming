-- what's the type of filter_concat?
filter_concat :: ([a] -> Bool) -> [[a]] -> [a]
filter_concat _ [] = []
filter_concat pred (x:xs)
  | pred x    = x ++ filter_concat pred xs
  | otherwise = filter_concat pred xs

-- Avoid recursion
filter_concat' :: ([a] -> Bool) -> [[a]] -> [a] 
filter_concat' pred = concat . filter pred
-- filter_concat' pred l = concat $ filter pred l

-- what's the type of take_while?
take_while:: (a -> Bool) -> [a] -> [a]
take_while _ [] = []
take_while pred (x:xs)
  | not(pred x) = []
  | otherwise    = x : take_while pred xs

-- rewrite (\l -> length l < 3) without using lambda expressions
-- (<3) . length

-- what's the type of your answer?
-- (<3) . length :: Foldable t => t a -> Bool