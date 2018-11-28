module Types where

type Name = String
type Language = String
type University = String

data Person = Programmer Name Language | Student Name University
                deriving (Show)

programmer = Programmer "Elton" "Haskell"
student = Student "Elton" "UFRN"

isProgrammer :: Person -> Bool
isProgrammer (Programmer _ _) = True
isProgrammer _                = False

isStudent :: Person -> Bool
isStudent (Student _ _) = True
isStudent _             = False
