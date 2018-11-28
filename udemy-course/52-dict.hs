module MyDict where

dict =
    [("python", "Guido van Rossum")
    ,("C", "Dennis Ritchie")
    ,("Haskell", "Haskell Curry")
    ]

findkey :: Eq k => k -> [(k, v)] -> Maybe v
findkey _ [] = Nothing
findkey key ((k,v):xs)
  | k == key = Just v
  | otherwise = findkey key xs
