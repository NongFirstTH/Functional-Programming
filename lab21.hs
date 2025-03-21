newtype State s a = State { runState :: s -> (a, s) }
type Queue a = ([a], [a], Int) -- front, back, size

size :: State (Queue a) Int
size = State $ \(f, b, n) -> (n, (f, b, n))

isEmpty :: State (Queue a) Bool
isEmpty = State $ \(f, b, n) -> (n == 0, (f, b, n))

enqueue :: a -> State (Queue a) ()
enqueue a = State $ \(f, b, n) -> ((), (f, a:b, n + 1))

dequeue :: State (Queue a) a
dequeue = State $ \(f, b, n) ->
    case f of
        (x:xs) -> (x, (xs, b, n - 1))
        []     -> case reverse b of
                    (y:ys) -> (y, (ys, [], n - 1))
                    []     -> error "Empty Queue!"

mkQueue :: [a] -> State (Queue a) ()
mkQueue l = State $ \_ -> ((), (l, [], length l))

empty :: State (Queue a) ()
empty = State $ \_ -> ((), ([], [], 0))

joinState :: State s (State s a) -> State s a
joinState outer = State $ \s ->
    let (inner, s') = runState outer s
    in runState inner s'

-- instance Functor (State s) where
--     fmap :: (a -> b) -> State s a -> State s b
--     fmap f (State g) = State $ \s ->
--         let (x, s') = g s in (f x, s')

-- prove that state monad satisfies functor laws
--  Law 1: fmap id = id 
--  fmap id (State g) = State $ \s -> let (a, s') = g s in (id a, s')
--                    = State $ \s -> let (a, s') = g s in (a, s')
--                    = State g

-- Law 2: fmap (g . f) = fmap g . fmap f
-- (fmap g . fmap f) (State h) = fmap g (fmap f (State h)) 
--                             = fmap g (State $ \s -> let (a, s') = h s in (f a, s'))
--                             = State $ \s -> let (b, s') = (\s -> let (a, s') = h s in (f a, s')) s in (g b, s')
--                             = State $ \s -> let (a, s') = h s in let (b, s'') = (f a, s') in (g b, s'')
--                             = State $ \s -> let (a, s') = h s in (g (f a), s')
--                             = State $ \s -> let (a, s') = h s in ((g . f) a, s')
--                             = fmap (g . f) (State h)
