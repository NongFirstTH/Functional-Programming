class  Functor f  where
    fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--     fmap = map

-- instance Functor Maybe where
--     fmap _ Nothing = Nothing
--     fmap f (Just a) = Just (f a)

-- instance Functor ((,) a) where
--     fmap f (x, y) = (x, f y)

-- data COp a = CVal Int a
--     deriving (Show)

-- instance Functor COp where
--   fmap f (CVal c v) = CVal (c+1) (f v)

instance Main.Functor ((->) r) where
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f x = f . x