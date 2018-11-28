leq :: [Int] -> [Int] -> Bool
leq [] [] = True
leq _ [] = False
leq [] _ = False
leq (x:xs) (y:ys) | x == y = leq xs ys
                  | otherwise = False
