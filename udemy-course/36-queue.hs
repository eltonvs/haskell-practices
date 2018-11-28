module QueueADT where

data Queue t = Q [t]
    deriving (Show)

newQueue :: Queue t
newQueue = Q []

pushQueue :: Queue t -> t -> Queue t
pushQueue (Q l) n = Q (l ++ [n])

popQueue :: Queue t -> Queue t
popQueue (Q [])     = error "Empty Queue"
popQueue (Q (_:xs)) = Q xs

topQueue :: Queue t -> t
topQueue (Q [])    = error "Empty Queue"
topQueue (Q (x:_)) = x

emptyQueue :: Queue t -> Bool
emptyQueue (Q []) = True
emptyQueue _      = False
