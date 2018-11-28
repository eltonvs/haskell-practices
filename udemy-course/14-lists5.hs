lcontains :: [Int] -> Int -> Bool
lcontains [] _ = False
lcontains (x:xs) y | x == y = True
                   | otherwise = lcontains xs y

bigger :: [Int] -> Int
bigger [x] = x
bigger (x:xs) | x > bigger xs = x
              | otherwise = bigger xs

leven :: [Int] -> Bool
leven [] = True
leven (x:xs) | mod x 2 == 0 = leven xs
             | otherwise = False
