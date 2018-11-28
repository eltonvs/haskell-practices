linv :: [t] -> [t]
linv [] = []
linv (x:xs) = linv xs ++ [x]
