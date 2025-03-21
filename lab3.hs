zipper' :: [a] -> [b] -> [(a,b)]
zipper' _ [] = []
zipper' [] _ = []
zipper' (x:xs) (y:ys) = (x , y) :zipper' xs ys

-- what's the type of zipper'?
-- zipper' :: [a] -> [b] -> [(a,b)]

-- what's the type of zipper' []?
-- zipper' [] :: [b] -> [(a, b)]

-- what does zipper' [] do?
-- รับ empty list มาก่อน จากนั้นจะรอรับ list ของ type อะไรก็ได้ แล้วจะได้ผลลัพธืเป็น empty list

-- is there a simpler way to express the same thing?
-- รับ empty list มาก่อน แล้วไม่ว่า list ข้างหลังจะเป็น type อะไร ผลลัพธ์ที่ได้จะเป็น empty list เสมอ

fac :: (Eq t, Num t) => t -> t
fac 0 = 1
fac n = n * fac (n-1)

-- what's the type of fac?
-- fac :: (Eq t, Num t) => t -> t

-- is your implementation of fac safe?
-- ไม่ ในกรณีที่ input มีค่าเยอะๆเช่น 100000 จะทำให้โปรแกรมค้างได้

-- what should you do to make it safer?
-- ใช้วิธี memo fac ที่จะคำนวณ fac n แล้วจำคำตอบไว้
-- หากอยากได้ผลลัพธ์ของ fac n ที่เคยคำนวณไว้แล้ว จะสามารถตอบได้ทันทีซึ่งลดเวลาในการคำนวณซ้ำๆได้มาก 
-- ทำให้โปรแกรมไม่ค้างจากการคำนวณ fac เดิมๆที่เคยคิดมาก่อนแล้ว
