ldrop :: Int -> [t] -> [t]
ldrop _ [] = []
ldrop 0 l = l
ldrop n (x:xs) = ldrop(n - 1) xs

mtake :: Int -> [t] -> [t]
mtake _ [] = []
mtake 0 _ = []
mtake n (x:xs) = x:mtake (n - 1) xs

mreplicate :: Int -> t -> [t]
mreplicate 0 _ = []
mreplicate n x = x:mreplicate (n - 1) x

msplitAt :: Int -> [t] -> ([t], [t])
msplitAt 0 l = ([], l)
msplitAt _ [] = ([], [])
msplitAt n (x:xs) = (x:fst(msplitAt (n - 1) xs), snd $ msplitAt (n - 1) xs)

mzip :: [a] -> [b] -> [(a, b)]
mzip [] _ = []
mzip _ [] = []
mzip (x:xs) (y:ys) = (x, y):mzip xs ys

munzip :: [(a, b)] -> ([a], [b])
munzip [] = ([], [])
munzip (x:xs) = (fst x:fst (munzip xs), snd x:snd (munzip xs))
