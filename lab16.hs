-- (,) r
-- (<*>) :: (r, a -> b) -> (r, a) -> (r, b)

-- how should we implement pure so that the initial label makes sense?
-- pure ก็รับ argrument มาอีก 1 ตัว แล้วเอามาสร้าง pair 
-- เขียนคร่าวๆได้ประมาณนี้ pure x = (r, x)

-- how should we implement (<*>) so that the resulting data contain appropriate label?
-- รับ pair 2 อัน โดยตัวแรกรับ pair ของ r และ function มา อีก pair รับ pair ของ r และ x 
-- แล้วทำ f x กับทางขวาของ pair
-- เขียนคร่าวได้ประมาณนี้
-- <*> (r, f) (r, x) = (r, f x)

-- *** prove ***
-- instance Applicative ((->) r) where
-- 	pure = const
-- 	(<*>) f g x = f x (g x)

-- (<*>) ::(r -> a -> b) -> (r -> a) -> (r -> b)
-- law 1: pure id <*> v = v
-- (pure id <*> v) x = (const id <*> v) x
                --   = const id x (v x)
                --   = id (v x)
                --   = v x

-- law 2: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- (pure (.) <*> u <*> v <*> w) x = (const (.) <*> u <*> v <*> w) x
--                                = ((const (.) x) (u x)) (v x) (w x)
--                                = ((.) (u x) (v x)) (w x)
--                                = (u x) ((v x) (w x))
--                                = (u x) ((v <*> w) x)
--                                = (u <*> (v <*> w)) x

-- law 3: pure f <*> pure x = pure (f x)
-- (pure f <*> pure x) r = ((const f) <*> (const x)) r
--                       = (const f r) ((const x) r)
--                       = f (x)
--                       = (const (f x)) r
--                       = (pure (f x)) r

-- law 4: u <*> pure y = pure ($ y) <*> u
-- (u <*> pure y) x = (u <*> (const y)) x
--                  = (u x) ((const y) x)
--                  = (u x) y
--                  = ($ y) (u x)
--                  = (const ($ y) x) (u x)
--                  = ((const ($ y)) <*> u) x
--                  = ((pure ($ y)) <*> u) x