class IfValue a where
    boolVal :: a -> Bool

instance IfValue Int where
    boolVal 0 = False
    boolVal _ = True

instance IfValue Double where
    boolVal 0 = False
    boolVal _ = True

instance IfValue Float where
    boolVal 0 = False
    boolVal _ = True

instance IfValue (Maybe a) where
    boolVal Nothing = False
    boolVal (Just _) = True

instance IfValue Bool where
    boolVal False = False
    boolVal True = True

instance IfValue [a] where
    boolVal [] = False
    boolVal _ = True

instance IfValue String where
    boolVal "" = False
    boolVal _ = True

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
    Nothing -> mapMaybe f xs
    Just x  -> x : mapMaybe f xs

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (a, b) = (f a, g b)
