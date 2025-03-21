-- data Gender = Male | Female
-- instance Eq Gender where
--     Male == Male = True
--     Female == Female = True
--     _ == _ = False

-- data Maybe' a = Nothing | Just a
-- instance Eq a => Eq (Maybe' a) where
--     Main.Nothing == Main.Nothing = True
--     Main.Just a == Main.Just b = a == b
--     _ == _ = False
    
-- data [] a = [] | a : [a]
-- instance Eq a => Eq [a] where
--     [] == [] = True
--     (x:xs) == (y:ys) = x == y && xs == ys
--     _ == _ = False

class IfValue a where
  boolVal :: a -> Bool

instance IfValue Int where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Float where
  boolVal 0.0 = False
  boolVal _ = True

instance IfValue Bool where
  boolVal False = False
  boolVal True = True

instance IfValue [a] where
  boolVal [] = False
  boolVal _ = True

instance IfValue String where
  boolVal "" = False
  boolVal _ = True

instance IfValue (Maybe a) where
    boolVal Nothing = False
    boolVal (Just _) = True

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing  = Nothing 
mapMaybe f (Just x) = Just (f x)

mapPair :: (b -> c) -> (a, b) -> (a, c)
mapPair f (a, b) = (a, f b)