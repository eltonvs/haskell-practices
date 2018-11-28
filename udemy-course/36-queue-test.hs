module QueueTest where

import QueueADT

runQueue :: Queue t -> [t]
runQueue queue
  | emptyQueue queue = []
  | otherwise = topQueue queue : runQueue (popQueue queue)
