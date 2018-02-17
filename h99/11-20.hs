module H99_11_20 where

-- Problem 11
data Item a = Single a | Multiple Int a
  deriving (Show)

encodeModified :: (Eq a) => [a] -> [Item a]
encodeModified []     = []
encodeModified (x:xs) = encodeModified' xs (1, x) where
  encodeModified' :: (Eq a) => [a] -> (Int, a) -> [Item a]
  encodeModified' [] (1, e) = [(Single e)]
  encodeModified' [] (c, e) = [(Multiple c e)]
  encodeModified' (x:xs) (c, e)
    | x == e    = encodeModified' xs (c + 1, e)
    | otherwise = (if c == 1 then (Single e) else (Multiple c e)) : rest where
      rest = encodeModified' xs (1, x)

-- Problem 12
decodeModified :: (Eq a) => [Item a] -> [a]
decodeModified []                  = []
decodeModified ((Single e):xs)     = e : decodeModified xs
decodeModified ((Multiple n e):xs) = repeat n e [] ++ (decodeModified xs) where
  repeat :: Int -> a -> [a] -> [a]
  repeat 0 _ acc = acc
  repeat n e acc = repeat (n - 1) e (e:acc)

-- Problem 13
-- Obs: my encodeModified function is already implemented directly
encodeDirect :: (Eq a) => [a] -> [Item a]
encodeDirect xs = encodeModified xs

-- Problem 14
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x:dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) n = replicate n x ++ (repli xs n)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n 1 where
  dropEvery' :: [a] -> Int -> Int -> [a]
  dropEvery' [] _ _ = []
  dropEvery' (x:xs) n c
    | mod c n == 0 = dropEvery' xs n (c + 1)
    | otherwise    = x : dropEvery' xs n (c + 1)

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split [] _     = ([], [])
split l 0      = ([], l)
split (x:xs) n = (x:first, second) where
  (first, second) = split xs (n - 1)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs s e = slice' xs s e 1 where
  slice' :: [a] -> Int -> Int -> Int -> [a]
  slice' [] _ _ _ = []
  slice' (x:xs) s e c
    | c >= s && c <= e = x : slice' xs s e (c + 1)
    | c < s            = slice' xs s e (c + 1)
    | otherwise        = []

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) n
  | n < 0     = rotate (x:xs) (length (x:xs) + n)
  | otherwise = rotate (xs ++ [x]) (n - 1)

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (e, x:ys) where
  (e, ys) = removeAt (n - 1) xs
