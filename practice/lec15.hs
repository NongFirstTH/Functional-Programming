-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- instance Main.Functor (Either a) where
--     fmap _ (Left a) = Left a
--     fmap f (Right a) = Right (f a)

maybeAp :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp Nothing _ = Nothing
maybeAp (Just f) a = 
    case a of
        Nothing -> Nothing
        Just a' -> Just (f a')

initMaybe :: a -> Maybe a
initMaybe a = Just a

listAp :: [a -> b] -> [a] -> [b]
listAp f a = [x y | x <- f, y <- a]

initList :: a -> [a]
initList a = [a]

-- (*3) (+100)
-- รอรับเลขมา +100 แล้วคูณ 3