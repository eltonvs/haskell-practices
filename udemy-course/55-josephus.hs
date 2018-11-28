module Josephus where

josephus :: Int -> Int -> Int
josephus 1 _ = 1
josephus n k = mod (k - 1 + josephus (n - 1) k) n + 1
