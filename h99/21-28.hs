module H99_21_28 where

import System.Random

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt e xs 1 = e:xs
insertAt e (x:xs) n = x : insertAt e xs (n - 1)

-- Problem 22
range :: Int -> Int -> [Int]
range s e = if s == e then [s] else s : range (s + 1) e

-- Problem 23
rnd_select = undefined

-- Problem 24
diff_select = undefined

-- Problem 25
rdm_permu = undefined

-- Problem 26
combinations = undefined

-- Problem 27
group = undefined

-- Problem 28
lsort = undefined
