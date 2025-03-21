-- class Functor f => Applicative f where
-- 	pure :: a -> f a
-- 	(<*>) :: f (a -> b) -> f a -> f b

-- instance Main.Applicative ((,) r) where
--     pure a = (mempty, a)
--     (<*>) :: (r, a -> b) -> (r, a) -> (r, b)
--     (<*>) (r1, f) (r2, a) = (r1 <> r2, f a)

-- instance Applicative ((->) r) where
-- 	pure = const
-- 	(<*>) f g x = f x (g x)

-- law 1: pure id <*> v = v
-- pure id <*> v = (const id) <*> v
-- = \x -> (const id x) (v x)
-- = \x -> id (v x)
-- = \x -> (v x)
-- = v
