maybeJoin :: Maybe (Maybe a) -> Maybe a
maybeJoin Nothing = Nothing
maybeJoin (Just Nothing) = Nothing
maybeJoin (Just (Just a)) = Just a

listJoin :: [[a]] -> [a]
listJoin [] = []
listJoin (x:xs) = x ++ listJoin xs

eitherJoin :: Either r (Either r a) -> Either r a
eitherJoin (Left r) = Left r
eitherJoin (Right (Left r)) = Left r
eitherJoin (Right (Right a)) = Right a

arrowJoin :: (r -> r -> a) -> r -> a
arrowJoin f r = f r r

pairJoin ::Semigroup r => (r, (r, a)) -> (r, a)
pairJoin (r1, (r2, a)) = (r1 <> r2, a)
-- what do we need to know about r?
-- r is Semigroup

-- instance Monad (Either e) where
--     	Right m >>= k = k m
--     	Left e  >>= _ = Left e

-- Prove Law1: return a >>= k = k a
--              Right a >>= k
--                        = k a
-- Prove Law2: m >>= return = m
--        Left e >>= return = Left e
--        Right m >>= return = return m
--                           = Right m
-- Prove Law3: m >>= (\x -> k x >>= h) = (m >>= k) >>= h
--        Left e >>= (\x -> k x >>= h) = Left e
--                                     = Left e >>= h
--                                     = (Left e >>= k) >>= h
--        Right m >>= (\x -> k x >>= h) = k m >>= h
--                                     = (return m >>= k) >>= h
--                                     = (Right m >>= k) >>= h

-- instance Monad []  where
-- xs >>= f = [y | x <- xs, y <- f x]

-- Prove Law1: return a >>= k = k a
--                  [a] >>= k
--                        = [y | x <- [a], y <- k x]
-- Prove Law2: m >>= return = m
--            [] >>= return = []
--           [a] >>= return = [y | x <- [a], y <- return x]
--                          = [y | x <- [a], y <- [x]]
-- Prove Law3: m >>= (\x -> k x >>= h) = (m >>= k) >>= h
--            [] >>= (\x -> k x >>= h) = []
--                                     = [] >>= h
--                                     = ([] >>= k) >>= h
--            [a] >>= (\x -> k x >>= h) = [y | x <- [a], y <- k x >> = h]
--                                      = [y | x <- [a], y <- [z | x <- k x, z <- h x]]
--                                      = [a] >>= k >>= h

-- instance Monad ((->) r) where
-- 	f >>= k = \ r -> k (f r) r

-- Prove Law1: return a >>= k = k a
--                    f >>= k = \ r -> k (f r) r
-- Prove Law2: m >>= return = m
-- f >>= return = \r -> return (f r) r
--                        = return a # Prove from law1 return a = f
--                        = f
-- Prove Law3: m >>= (\x -> k x >>= h) = (m >>= k) >>= h
--            f >> = (\x -> k x >>= h) = (\ r -> k (f r) r) >>= h           
--                                     = (f >>= k) >>= h