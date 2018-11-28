module Review where

concat :: [a] -> [a] -> [a]
concat [] l2 = l2
concat l1 [] = l1
concat (l1:l1s) l2 = l1 : Review.concat l1s l2

inv :: [a] -> [a]
inv [] = []
inv (x:xs) = inv xs ++ [x]

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + Review.sum xs

tail :: [a] -> [a]
tail [] = []
tail (_:xs) = xs

type Person = String
type Car = String
type Age = Int
type Register = (Person, Car, Age)
type DataBase = [Register]

getPerson :: Register -> Person
getPerson (p, _, _) = p

getCar :: Register -> Car
getCar (_, c, _) = c

getDBCars :: DataBase -> [Car]
getDBCars [] = []
getDBCars(x:xs) = getCar x : getDBCars xs
