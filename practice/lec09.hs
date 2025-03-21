numericGrade "A"  = 4.0
numericGrade "B+" = 3.5
numericGrade "B"  = 3.0
numericGrade "C+" = 2.5
numericGrade "C"  = 2.0
numericGrade "D+" = 1.5
numericGrade "D"  = 1.0
numericGrade "F"  = 0.0

gpa grades =
  average
  . map numericGrade
  . filter isNumerableGrade
  $ grades

average xs = sum xs / fromIntegral(length xs)

isNumerableGrade =
  (`elem` numerableGrades)
numerableGrades = ["A", "B+", "B", "C+", "C", "D+", "D", "F"]

foldl' :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl' f acc []     = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

suM :: Num a => [a] -> a
suM [] = 0
suM (x:xs) = x + suM xs

sum' :: (Foldable t, Num a) => t a -> a
sum' l = foldl (+) 0 l

concaT :: [[a]] -> [a]
concaT [] = []
concaT (x:xs) = x ++ concaT xs

concat' :: Foldable t => t [a] -> [a]
concat' l = foldl (++) [] l

foldr' :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

sumr l = foldr (+) 0 l
conr l = foldr (++) [] l

revL :: Foldable t => t a -> [a]
revL l = foldl (\acc x -> x:acc) [] l
revR :: Foldable t => t a -> [a]
revR l = foldr (\x acc -> acc ++ [x]) [] l

mapL f l = reverse $ foldl (\acc x -> f x : acc) [] l
mapR f l = foldr (\x acc -> f x : acc) [] l