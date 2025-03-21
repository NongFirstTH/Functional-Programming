-- rewrite list reverse using left fold
reverseL :: [a] -> [a]
reverseL l = foldl (\acc x -> x:acc) [] l

-- rewrite list reverse using right fold
reverseR :: [a] -> [a]
reverseR l = foldr (\x acc -> acc ++ [x]) [] l

-- which version is more efficient, and why?
-- reverse left มีประสิทธิภาพมากกว่า เพราะ เป็น tail recursion ในที่นี้คือการนำสมาชิกตัวหน้าของ list ที่เหลือมาต่อกับ list คำตอบของเราแล้วได้ผลลัพธ์เลย
-- ส่วน reverse right จะต้อง recursive ตัวเองเรื่อยๆ แล้วจึงค่อยนำสมาชิกแต่ละตัวมาต่อกับ list คำตอบ
-- ถ้าพิจารณาจาก time complexity จะได้ว่า
-- reverse left ใช้ (:) ในการต่อ list สมาชิกตัวหน้ากับ list คำตอบ ใช้เวลา O(1) มีทั้งหมด n ตัว ใช้ O(n)
-- reverse right ใช้ (++) ในการต่อ list สมาชิกที่ต้องเรียกตัวเอง n ครั้งซึ่งคือ O(n) กับ list คำตอบทั้งหมด n ตัวจึงใช้ O(n^2)

-- rewrite map using fold
map :: (a -> b) -> [a] -> [b] 
map f l = reverse $ foldl (\acc x -> f x:acc) [] l

-- rewrite filter using fold
filter :: (a -> Bool) -> [a] -> [a] 
filter f l = reverse $ foldl (\acc x -> if f x then x:acc else acc) [] l