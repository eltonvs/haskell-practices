module Enum where

data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         deriving (Eq, Show)

getDay :: Int -> Day
getDay n = case n of
             1 -> Sunday
             2 -> Monday
             3 -> Tuesday
             4 -> Wednesday
             5 -> Thursday
             6 -> Friday
             7 -> Saturday
