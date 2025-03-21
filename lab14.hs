data COp a = CVal Int a deriving (Show)

instance Functor COp where
  fmap f (CVal c v) = CVal (c+1) (f v)

-- is this an appropriate definition of a functor for COp?
-- No เพราะว่า เมื่อให้ f = id จากกฏข้อแรกของ Functor คือ fmap id x = x
-- แต่เมื่อลองนำ instance ดังกล่าวไปใช้ เช่น fmap id (CVal 0 1) = CVal 1 1
-- จะเห็นได้ว่าผลลัพธ์ก่อนทำของ CVal และหลังทำไม่ใช่ตัวเดิม การนิยามแบบนี้จึงไม่เหมาะสม

-- instance Functor ((->) r) where
    -- fmap f s = f . s 

-- Law 1: fmap id s = id . s = s
-- Law 2: fmap (g . f) s =  (g . f)  s
--   fmap g . (fmap (f . s))
--   fmap g . (fmap (f . s)) = fmap g . (f . s)
--                           = g . f . s
--                           = (g . f) . s