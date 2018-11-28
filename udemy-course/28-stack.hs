module Stack where

push :: [t] -> t -> [t]
push list el = list ++ [el]

pop :: [t] -> [t]
pop []     = error "Empty Stack"
pop [e]    = []
pop (x:xs) = x:pop xs

top :: [t] -> t
top []     = error "Empty Stack"
top [e]    = e
top (x:xs) = top xs

empty :: [t] -> Bool
empty [] = True
empty _  = False
