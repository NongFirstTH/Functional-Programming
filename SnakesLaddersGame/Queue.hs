module Queue where

import GHC.Generics (Generic)

data Queue a = Queue { front :: [a], back :: [a] }
    deriving (Show, Generic)

newQueue :: Queue a
newQueue = Queue [] []

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _             = False

headQ :: Queue a -> a
headQ (Queue [] []) = error "Can't find top of an empty queue"
headQ (Queue (x:_) _) = x
headQ (Queue [] ys) = headQ (Queue (reverse ys) [])

tailQ :: Queue a -> a
tailQ (Queue [] []) = error "Can't find top of an empty queue"
tailQ (Queue _ (y:_)) = y
tailQ (Queue xs []) = tailQ (Queue [] (reverse xs))

enq :: Queue a -> a -> Queue a
enq (Queue xs ys) y = Queue xs (y:ys)

deq :: Queue a -> (a, Queue a)
deq (Queue [] []) = error "Can't deq from an empty queue"
deq (Queue (x:xs) ys) = (x, Queue xs ys)
deq (Queue [] ys) = deq (Queue (reverse ys) [])
