newtype Void = Void Void

-- False ⇒ A
absurd :: Void -> a
absurd (Void x) = absurd x

--    (A ⇒ B ⇒ C) ⇒ (A ⇒ B) ⇒ A ⇒ C
p1 :: (a -> b -> c) -> (a -> b) -> a -> c
p1 g f a =g a $ f a  

--    (A ∧ B) => A ∨ B
p2 :: (a, b) -> Either a b
p2 (_, b) = Right b

--    ((A ⇒ B) ∧ A) ⇒ B
p3 :: (a -> b, a) -> b
p3 (f, a) = f a

--    A ∨ B ⇒ A ⇒ B
p4 :: Either (a -> Void) b -> a -> b
p4 (Right b) _ = b
p4 (Left f) a = absurd (f a) 