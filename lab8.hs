-- what's the type of partition?
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p [] = ([], [])
partition p (x:xs)
  | p x       = (x:l, r)
  | otherwise = (l, x:r)
  where (l, r) = partition p xs
-- what does partition do?
-- เก็บผลลัพธ์ที่เป็นจริงกับ predicate p มาเก็บไว้ที่ list ข้างซ้าย
-- นอกเหนือจากนี้เก็บไว้ใน list ก้อนขวา จะนั้นมาใส่เป็น pair ของ list ซ้าย และ list ขวา

-- rewrite filter using partition
filter :: (a -> Bool) -> [a] -> [a]
filter p l = fst $ partition p l

-- rewrite quicksort
qsort :: Ord a => [a] -> [a]
qsort []      = []
qsort (hd:tl) = qsort l ++ [hd] ++ qsort r
  where 
    l = fst $ partition (<hd) tl
    r = fst $ partition (>=hd) tl

-- Look up type Ordering
-- what is it for?
-- ใช้แสดงผลลัพธ์จากการเปรียบเทียบของค่า 2 ค่า ซึ่งผลลัพธ์ที่ว่าจะแสดงได้ 3 แบบ คือ 
-- LT(Less than)    ใช้ระบุว่าผลลัพธ์ของการเปรียบเทียบ ค่าตัวแรกน้อยกว่าค่าที่สอง
-- EQ(Equal)        ใช้ระบุว่าผลลัพธ์ของการเปรียบเทียบ ค่าตัวแรกเท่ากับค่าที่สอง
-- GT(Greater than) ใช้ระบุว่าผลลัพธ์ของการเปรียบเทียบ ค่าตัวแรกมากกว่าค่าที่สอง

-- how many constructors are there?
-- 3

-- how many ways can we pattern-match an Ordering value?
-- 3 ways consists of LT, EQ, GT

-- gpa
-- A, B+, B, C+, C, D+, D, F, W
-- assume every course has 3 credits
mapGrade::Fractional a => String -> a
mapGrade grade
    | grade == "A" = 4.0
    | grade == "B+" = 3.5
    | grade == "B" = 3.0
    | grade == "C+" = 2.5
    | grade == "C" = 2.0
    | grade == "D+" = 1.5
    | grade == "D" = 1.0
    | grade == "F" = 0.0
    | grade == "W" = 0.0
    | otherwise = error "not a grade"

gpa_list :: Fractional a => [String] -> [a]
gpa_list [] = []
gpa_list (x:xs) = (mapGrade x)*3 : gpa_list xs

-- what's the type of gpa?
gpa :: Fractional a => [String] -> a
gpa l = sum (gpa_list l) / fromIntegral(length l * 3)