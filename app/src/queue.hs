module Queue where

import Control.Monad.State

--
-- Uses Rafal Szymanski's implementation from his post here:
-- http://rafal.io/posts/haskell-queues.html
--

data Queue a = Queue { 
  inbox :: [a], 
  outbox :: [a] 
} deriving (Eq, Show)

emptyQueue = Queue [] []

push :: a -> Queue a -> Queue a
push e (Queue inb out) = Queue (e:inb) out

pop :: Queue a -> (Maybe a, Queue a)
pop q = 
  case top of
    Nothing   -> (top, emptyQueue)
    Just elem -> (Just elem, poppedQueue)
    where
      (top, q') = peek q
      poppedQueue = Queue (inbox q') (tail $ outbox q')

peek :: Queue a -> (Maybe a, Queue a)
peek (Queue [] [])    = (Nothing, emptyQueue)
peek (Queue inb [])   = peek $ Queue [] (reverse inb)
peek q@(Queue _ outb) = (Just $ head outb, q)


-- QueueState for more simply manipulating Queue

type QueueState a = State (Queue a)

pushQueue :: a -> QueueState a ()
pushQueue e = state $ \q -> ((),push e q)

popQueue :: QueueState a (Maybe a)
popQueue = state $ \q -> pop q
