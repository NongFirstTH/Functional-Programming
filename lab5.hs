list_map :: (t -> a) -> [t] -> [a]
list_map _ [] = []
list_map f (x:xs) = f x : list_map f xs

-- list_map' tail recursion
list_map' :: (t -> a) -> [t] -> [a]
list_map' f l = reverse $ list_map_aux l []
  where
    list_map_aux [] res = res
    list_map_aux (x:xs) res =  list_map_aux xs (f x:res)

-- Three more test cases
-- 1. list_map' (/2) [1,2,3] = [0.5,1.0,1.5]
-- 2. list_map' ('A':) ["pple","nt"] = ["Apple","Ant"]
-- 3. list_map' (head) ["Fish","People","=", "Abc","+/*"] "FP=A+"

-- zipper' tail recursion
zipper' :: [a] -> [b] -> [(a, b)]
zipper' l1 l2 = zipper_aux l1 l2 []
  where
    zipper_aux [] _ res  = reverse res
    zipper_aux _ [] res= reverse res
    zipper_aux (x:xs) (y:ys) res = zipper_aux xs ys ((x, y): res)

fibs :: [Integer]
fibs = 1: 1: zipWith (+) fibs (tail fibs)