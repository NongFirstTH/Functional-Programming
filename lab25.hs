pairBimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
pairBimap f g (a, b) = (f a, g b)

-- prove that these bifunctor laws hold:
-- pairBimap id id = id
-- pairBimap id id = \(a, b) -> (id a, id b)
-- pairBimap id id = \(a, b) -> id (a, b)
-- pairBimap id id = id

-- pairBimap (f . g) (h . i) = pairBimap f h . pairBimap g i
-- pairBimap (f . g) (h . i) = \(a, b) -> ((f . g) a, (h . i) b)
-- pairBimap (f . g) (h . i) = \(a, b) -> (f (g a), h (i b))
-- pairBimap (f . g) (h . i) = \(a, b) -> (pairBimap f h (g a, i b))
-- pairBimap (f . g) (h . i) = \(a, b) -> (pairBimap f h . pairBimap g i) (a, b)
-- pairBimap (f . g) (h . i) = pairBimap f h . pairBimap g i

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]
 
-- TRUE
-- for any f :: a -> b,
-- fmap f . maybeToList = maybeToList . fmap f
-- case Nothing
-- fmap f (maybeToList Nothing) = fmap f [] = []

