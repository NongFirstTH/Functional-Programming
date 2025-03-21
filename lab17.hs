-- more examples of monoids

-- carrier: set of integers
-- operator: (**), identity: 1

-- carrier: list of integers
-- operator: (++), identity: []

-- carrier: functions from integers to integers
-- operator: (.), identity: id

newtype And = And { getAnd :: Bool } deriving Show

instance Semigroup And where
  (And x) <> (And y) = And (x && y)

instance Monoid And where
  mempty = And True

newtype Or = Or { getOr :: Bool } deriving Show

instance Semigroup Or where
  (Or x) <> (Or y) = Or (x || y)

instance Monoid Or where
    mempty = Or False

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just x) f = f x

listBind :: [a] -> (a -> [b]) -> [b]
listBind [] _ = []
listBind (x:xs) f = f x ++ listBind xs f

eitherBind :: Either r a -> (a -> Either r b) -> Either r b
eitherBind (Left a) _ = Left a
eitherBind (Right a) f = f a

arrowBind :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
arrowBind a f = \r -> f (a r) r

pairBind :: (r, a) -> (a -> (r, b)) -> (r, b)
pairBind a f = f $ snd a
-- what do we need to know about r?
-- r can be anything type