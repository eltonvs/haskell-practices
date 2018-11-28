module MapFilter where

isPrime :: Int -> Bool
isPrime n = [x | x <- [2..(round $ sqrt $ fromIntegral n)], mod n x == 0] == []

primeNumbers :: [Int]
primeNumbers = filter isPrime [1..100]
