iseven :: Int -> Bool
iseven x = mod x 2 == 0

evens :: [Int]
evens = [x | x <- [1..], iseven x]
