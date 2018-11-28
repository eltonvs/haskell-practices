-- Question 01
multiples :: Int -> [Int] -> [Int]
multiples _ [] = []
multiples n (x:xs)
  | x > n && mod x 3 == 0 = x : multiples n xs
  | otherwise = multiples n xs

-- Question 02
nfirst :: Int -> [a] -> [a]
nfirst _ []     = []
nfirst 0 _      = []
nfirst n (x:xs) = x : nfirst (n - 1) xs

-- Question 03
intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) ys
  | contains x ys = x : intersect xs' [y | y <- ys, y /= x]
  | otherwise = intersect xs' ys
  where
    xs' = [x' | x' <- xs, x /= x']
    contains :: (Eq a) => a -> [a] -> Bool
    contains _ [] = False
    contains e (x:xs)
      | e == x = True
      | otherwise = contains e xs

-- Question 04
isprime :: Int -> Bool
isprime 1 = False
isprime n = length [x | x <- [2..round $ sqrt $ fromIntegral n], mod n x == 0] == 0

fact :: Int -> Int
fact n = fact' n 1 where
  fact' :: Int -> Int -> Int
  fact' 0 acc = acc
  fact' n acc = fact' (n - 1) (n*acc)

fib :: Int -> Int
fib n = fib' n 0 1 where
  fib' :: Int -> Int -> Int -> Int
  fib' 0 n1 n2 = n1
  fib' n n1 n2 = fib' (n - 1) n2 (n1 + n2)

fib_lst :: Int -> [Int]
fib_lst n = [fib x | x <- [0..n]]

main = undefined
