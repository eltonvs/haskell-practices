module MyModule where

isEven :: Int -> Bool
isEven n = mod n 2 == 0

isOdd :: Int -> Bool
isOdd n = not $ isEven n
