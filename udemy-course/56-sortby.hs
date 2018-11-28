module SortBy where

import Data.List

mylist = [("Thais", 21), ("Elton", 19), ("Carlos", 20), ("Yuri", 20)]

mylist_s = sortBy sortBy' mylist where
  sortBy' :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
  sortBy' (_, a1) (_, a2)
    | a1 < a2   = LT
    | a1 > a2   = GT
    | otherwise = EQ
