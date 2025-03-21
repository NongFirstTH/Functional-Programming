filter_concat :: ([a] -> Bool) -> [[a]] -> [a]
filter_concat pred l = concat $ filter pred l
-- filter_concat = \pred l -> concat (filter pred l)
-- filter_concat = \pred l -> concat ((filter pred) l)
-- filter_concat = \pred l -> (concat . (filter pred)) l
-- filter_concat = \pred -> (concat . (filter pred))
-- filter_concat = \pred -> concat . (filter pred)
-- filter_concat = \pred -> (.) concat (filter pred)
-- filter_concat = \pred -> ((.) concat) (filter pred)
-- filter_concat = \pred -> (((.) concat) . filter) pred
-- filter_concat = ((.) concat) . filter
-- filter_concat = (concat .) . filter

-- \oper pred l -> map oper (filter pred l)
-- \oper pred -> (map oper) . (filter pred)
-- \oper pred -> ((.)(map oper)) (filter pred)
-- \oper -> ((.)(map oper)) . (filter)
-- \oper -> (.) ((.)(map oper)) filter
-- \oper -> (. filter) ((.)(map oper))
-- (. filter) . (.) . map
a = [x^2 | x <- [1..10]]
b = [(x,y) | x <- [2,3,5], y <- [1,2,4], even $ x+y]

ap f1 f2 l = f1 l (f2 l)
g = \f l -> l ++ map f l
-- g = \f -> ap (++) (map f)
-- g = \f -> ((ap (++)). (map)) f
-- g = (ap (++)). (map)
-- g = (.) (ap (++)) (map)

-- contains1 = \x l -> any (x<) l
-- contains1 = \x -> any (x <)
-- contains1 = \x -> any ((<) x)
-- contains1 = \x -> (any . (<)) x 
-- contains1 = any . (<)

-- contains2 = \l x -> any (x<) l
-- contains2 = \x l ->flip $ any (x<) l
-- contains2 = \x ->flip $ any (x<)
-- contains2 = \x ->flip $ any ((<)x)
-- contains2 = \x ->flip $ (any . (<)) x
-- contains2 = flip $ any . (<)
-- [(x,y) | x <- [2,3,5], y <- [1,2,4], even $ x+y]

pair x y = concat $ map (\f -> map f y) $ map (,) x
listPair x y = filter (even . uncurry (+)) $ pair x y