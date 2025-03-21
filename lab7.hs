-- c1 x =  \l -> any (>x) l
-- c1 x = any (>x)
c1 x =  (any . (>)) x

-- c2 l =  \x -> any (>x) l
-- c2 x = any (>x)
c2 x =  (any . (>)) x

len_comp l = sum [1 | i <- l]

-- [(x,y) | x <- [2,3,5], y <- [1,2,4], even $ x+y]
-- [(2,2),(2,4),(3,1),(5,1)]

f = flip map [2, 3, 5] 
    (\x -> map (\y -> (x, y)) 
        (
           filter (\y -> even (x + y)) [1, 2, 4]
        )
    )