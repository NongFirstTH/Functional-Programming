join :: ([a], [a]) -> [a]
join ([], ys) = ys
join (x:xs, ys) = x : join (xs, ys)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = join (rev xs, [x])

-- how long does join take?
-- O(n) โดย n เป็น input size of the left list
-- มาจาก join จะไปทำ : กับ left list ไปจนครบทุกตัวซึ่งได้ O(n) 
-- ส่วน right list ได้ O(1) เพราะ join([], ys) = ys

-- how long does your rev take to compute the reverse of a list
-- O(n*n) โดย n เป็น size of the list
-- rev จะใช้เวลา O(n) เพราะ ต้อง reverse n - 1 ตัว
-- join จะใช้เวลา O(n) เพราะ ต้อง join จากที่ rev มาซึ่งมี n - 1 ตัว  
-- join ของ list ที่ rev จึงเป็น O(n*n)

-- are you satisfied with the running time?
-- ไม่พอใจ improve โดยใช้วิธีคือ แบ่ง list เป็น 2 ก้อน เท่าๆกัน แบ่งไปเรื่อยๆจนถึง list ที่มีขนาดเล็กที่สุด 
-- จากนั้นแต่ละก้อนไป reverse มาให้เสร็จแล้วมาประกอบกัน ทีละครึ่งๆ ทีละครึ่งๆ 
-- มี n ตัว และต้องแบ่งทีละครึ่งใช้ lg n ดังนั้นจึงใช้เวลา O(nlgn)   

-- fib :: (Integral t, Integral a, Ord t) => t -> a
-- fib n
--  | n < 0 = error "negative number"
--  | n == 0 = 0
--  | n == 1 = 1
--  | otherwise = fib (n-1) + fib (n-2)

-- what's the type of your fib?
-- fib :: (Integral t, Integral a, Ord t) => t -> a

-- how long does your fib take to compute fib n?
-- O(2^n) มาจากแต่ละครั้งจะแบ่ง fib เป็น 2 ก้อน เช่น fib n = fib (n-1) + fib (n-2)
-- แบ่งจนกระทั่งถึง base case จะเห็นว่าเป็น binary tree ที่สูง n แต่ละครั้งจะแบ่งทีละ 2 ก้อนจึงได้ 2^n

-- are you satisfied with the running time?
-- ไม่พอใจ improve โดยใช้วิธี memo fibo คือ จะจำผลลัพธ์จากที่เคยคำนวณไว้แล้วสามารถนำมาใช้ต่อได้ โดยไม่ต้องคำนวณซ้ำๆอีก
-- แต่ละ fib จะคำนวณแค่ 1 ครั้ง มี n ครั้ง ก็ใช้ O(n)

fib :: (Eq p, Num t, Num p) => p -> t
fib n = fib_aux 0 0 1
  where
    fib_aux i res res'
      | i == n    = res
      | otherwise = fib_aux (i+1) res' (res+res')
