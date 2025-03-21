-- carrier: set of even integers
-- operation: (+)
-- identity: (0)

-- carrier: set of Boolean = {True, False}
-- operation: ||
-- identity: False

newtype AND = AND {getAND :: Bool}
instance Semigroup AND where
    (AND a) <> (AND b) = AND (a && b)
instance Monoid AND where
    mempty = AND True

newtype OR = OR {getOR :: Bool}
instance Semigroup OR where
    (OR a) <> (OR b) = OR (a || b)
instance Monoid OR where
    mempty = OR False

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
arrowBind u f = \r -> f (u r) r

pairBind ::Semigroup r => (r, a) -> (a -> (r, b)) -> (r, b)
pairBind (r1, x) f = (r1 <> r2 , y)
    where (r2, y) = f x