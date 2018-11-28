ifeven :: Int -> Bool
ifeven n = if mod n 2 == 0 then True else False

caseeven :: Int -> Bool
caseeven n = case mod n 2  == 0 of
                True -> True
                False -> False
