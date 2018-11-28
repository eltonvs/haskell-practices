inv :: [t] -> [t]
inv [] = []
inv (x:xs) = inv(xs) ++ [x]
