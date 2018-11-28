module Set(Set, new, add, exists, empty, remove) where

data Set t = S [t]
    deriving (Show, Ord, Eq)

myset = S [1, 2, 3, 4]

new :: Set t
new = S []

add :: Eq t => Set t -> t -> Set t
add (S l) e
  | exists (S l) e = S l
  | otherwise = S $ e:l

exists :: Eq t => Set t -> t -> Bool
exists (S []) _ = False
exists (S (x:xs)) e
  | x == e = True
  | otherwise = exists (S xs) e

empty :: Set t -> Bool
empty (S []) = True
empty _    = False

remove :: Eq t => Set t -> t -> Set t
remove (S []) _ = S []
remove (S (x:xs)) e
  | x == e = S xs
  | otherwise = S $ x:xss
      where (S xss) = remove (S xs) e
