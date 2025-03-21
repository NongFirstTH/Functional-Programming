maybeJoin :: Maybe (Maybe a) -> Maybe a
maybeJoin Nothing = Nothing
maybeJoin (Just x) = x

listJoin :: [[a]] -> [a]
listJoin [] = []
listJoin (x:xs) = x ++ listJoin xs

eitherJoin :: Either r (Either r a) -> Either r a
eitherJoin (Left a) = Left a
eitherJoin (Right a) = a

arrowJoin :: (r -> r -> a) -> r -> a
arrowJoin f r = f r r

pairJoin :: Semigroup r => (r, (r, a)) -> (r, a)
pairJoin (r1, (r2, a)) = (r1 <> r2, a)
