module BinarySearch where

bsearch :: Ord a => a -> [a] -> Int
bsearch e v = bsearch_aux e v 0 (length v - 1)

bsearch_aux :: Ord a => a -> [a] -> Int -> Int -> Int
bsearch_aux _ [] _ _ = -1
bsearch_aux el v start end
  | start > end = -1
  | v !! mid == el = mid
  | v !! mid < el = bsearch_aux el v (mid + 1) end
  | v !! mid > el = bsearch_aux el v start (mid - 1)
  where mid = div (start + end) 2
