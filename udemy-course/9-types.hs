type Name = String
type Age = Int
type Language = String
type Person = (Name, Age, Language)

person :: Person
person = ("Elton", 19, "Haskell")

get_name :: Person -> Name
get_name (name, _, _) = name

get_age :: Person -> Age
get_age (_, age, _) = age

get_lang :: Person -> Language
get_lang (_, _, lang) = lang
