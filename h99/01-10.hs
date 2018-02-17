module H99_01_10 where

-- Problem 1
myLast :: [t] -> t
myLast [x]    = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [t] -> t
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt :: [t] -> Int -> t
elementAt [] _     = error "Index out of bounds"
elementAt (x:_) 1  = x
elementAt (_:xs) n = elementAt xs (n - 1)

-- Problem 4
myLength :: [t] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [t] -> [t]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: (Eq t) => [t] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome (x:xs)
  | x == last xs = isPalindrome $ init xs
  | otherwise    = False

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a)      = [a]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress []  = []
compress [a] = [a]
compress (x:xs)
  | x == head xs = compress xs
  | otherwise    = x:compress xs

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = p:pack r where
  (p, r) = pack' x [] (x:xs)
  pack' :: (Eq a) => a -> [a] -> [a] -> ([a], [a])
  pack' _ l [] = (l, [])
  pack' y l (x:xs)
    | y == x    = pack' y (x:l) xs
    | otherwise = (l, x:xs)

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode []     = []
encode (x:xs) = encode' xs (1, x) where
  encode' :: (Eq a) => [a] -> (Int, a) -> [(Int, a)]
  encode' [] acc = [acc]
  encode' (x:xs) (c, e)
    | x == e    = encode' xs (c + 1, e)
    | otherwise = (c, e) : encode' xs (1, x)
