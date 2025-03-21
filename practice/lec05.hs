rev :: [a] -> [a]
rev l = revAux l []
    where
        revAux [] res = res
        revAux (x:xs) res = revAux xs (x:res)

sum' :: Num a => [a] -> a
sum' l = sumAux l 0
    where
        sumAux [] res = res
        sumAux (x:xs) res = sumAux xs (res+x)

fac :: Integral p => p -> p
fac n
    | n >= 0 = facAux n 1
    | otherwise = error "Negative number"   
    where
        facAux 0 res = res
        facAux n res = facAux (n-1) (n*res)

-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n-1) + fib (n-2)
fib :: Integral a => a -> a
fib n = fibAux 0 0 1
    where 
    fibAux i res res'
     | i == n = res
     | otherwise = fibAux (i+1) res' (res+res')

list_map :: (a -> b) -> [a] -> [b]
list_map _ [] = []
list_map f (x:xs) = f x : list_map f xs

list_map' :: (a -> b) -> [a] -> [b]
list_map' f l = mapAux l []
    where
        mapAux [] res = res
        mapAux (x:xs) res = mapAux xs (f x : res)

zipper :: [a] -> [b] -> [(a, b)]
zipper [] _ = []
zipper _ [] = []
zipper (x:xs) (y:ys) = (x,y) : zipper xs ys

zipper' :: [a] -> [b] -> [(a, b)]
zipper' x y = reverse $ zipAux x y []
    where
    zipAux _ [] res = res
    zipAux [] _ res = res
    zipAux (x:xs) (y:ys) res = zipAux xs ys ((x,y) : res)

fibs = 1: 1: zipWith (+) fibs (tail fibs)