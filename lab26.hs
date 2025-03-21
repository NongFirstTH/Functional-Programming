s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x _ = x

i :: a -> a 
i x = x

comp :: (b->c) -> (a->b) -> (a->c)
comp = s . k
-- comp g f = s (k g) f = \a -> s (k g) f a = \a -> g (f a) = g . f

revApp :: a -> (a->b) -> b
revApp = flip' $ s k i 
-- revApp a f = s k i (f a) = k (i (f a)) a = k (f a) a = f a

-- My flip version 
flip' :: (a -> b -> c) -> b -> a -> c 
flip' = s (f) . k
-- flip' f b a =  = s f (k b) a = f a b 

f :: b -> (a -> b -> c) -> a -> c
f b g a = s g (k b) a 
-- f b g a = s (s (k b)) (k b) a = s g (k b) a = g a b


-- flip ยากเกินทำไม่ได้ครับ เลยไปเอาของคนอื่นมา
-- ref: https://stackoverflow.com/questions/29379277/convert-flip-lambda-into-ski-terms
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' = s (s (k s) (s (s (k s) (s (k k) (k s))) k)) (k k)
-- S [S (K S) (S (S (K S) (S (K K) (K S))) K)] [K K] f x y
-- -> S (K S) (S (S (K S) (S (K K) (K S))) K) f (K K f) x y
-- -> K S f (S (S (K S) (S (K K) (K S))) K f) (K K f) x y
-- -> S [S (S (K S) (S (K K) (K S))) K f] (K K f) x y
-- -> S [S (K S) (S (K K) (K S))] K f x (K K f x) y
-- -> S [K S] [S (K K) (K S)] f (K f) x (K K f x) y
-- -> K S f (S (K K) (K S) f) (K f) x (K K f x) y
-- -> S [S (K K) (K S) f] [K f] x (K K f x) y
-- -> S [K K] [K S] f x (K f x) (K K f x) y
-- -> K K f (K S f) x (K f x) (K K f x) y
-- -> K (K S f) x (K f x) (K K f x) y
-- -> K S f (K f x) (K K f x) y
-- -> S [K f x] [K K f x] y
-- -> K f x y (K K f x y)
-- -> f y (K K f x y)
-- -> f y (K x y)
-- -> f y x