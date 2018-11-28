module Sort where

import Data.List

type Name = String
type Language = String
data Person = Programmer Name Language
    deriving (Eq, Ord, Show)

p1 = Programmer "Elton" "Haskell"
p2 = Programmer "Thais" "Java"
p3 = Programmer "Carlos" "None"

plist = [p1, p2, p3]

sorted = sort plist
