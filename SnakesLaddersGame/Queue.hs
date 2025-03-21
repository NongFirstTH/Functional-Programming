-- http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2007/02/08/haskell-queues-without-pointers/

{-# OPTIONS_GHC -Wall #-}
module Queue where

data Queue a = Queue [a] [a]
    deriving (Show)

-- Create a new queue.
newQueue :: Queue a
newQueue = Queue [] []

-- Test whether a queue is empty.
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

-- Add an item to the back of the queue, returning the updated queue.
enq :: Queue a -> a -> Queue a
enq (Queue xs ys) y = Queue xs (y:ys)

-- Remove an item from the front of the queue, returning the item and the updated queue.
deq :: Queue a -> (a, Queue a)

-- If the queue is empty, raise an error.
deq (Queue [] []) = error "Can't deq from an empty queue"

-- If there's at least one item in the front part of the queue, return it.
deq (Queue (x:xs) ys) = (x, Queue xs ys)

-- If the front part is empty, reverse the back part, move it to the front, and try again.
deq (Queue [] ys) = deq (Queue (reverse ys) [])