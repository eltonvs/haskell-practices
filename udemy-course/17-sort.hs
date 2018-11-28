lsort :: [Int] -> [Int]
lsort [] = []
lsort xs = find_min xs : lsort(remove_min xs)

find_min :: [Int] -> Int
find_min [x] = x
find_min (x:xs) | x < find_min xs = x
                | otherwise = find_min xs

remove_min :: [Int] -> [Int]
remove_min [] = []
remove_min (x:xs) | x == find_min(x:xs) = xs
                  | otherwise = x : remove_min xs
