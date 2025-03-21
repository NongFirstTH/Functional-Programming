newtype State s a = State { runState :: s -> (a, s) }

joinState :: State s (State s a) -> State s a
joinState (State g) = State $ \s ->
    let (State h, s') = g s
    in h s'