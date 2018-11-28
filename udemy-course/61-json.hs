module Json where

data JData = JString String
           | JNumber Double
           | JBool Bool
           | JNull
           | JObject [(String, JData)]
           | JArray [JData]
           deriving (Eq, Ord, Show)

getString :: JData -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JData -> Maybe Int
getInt (JNumber n) = Just $ truncate n
getInt _           = Nothing

getDouble :: JData -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool :: JData -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JData -> Maybe [(String, JData)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JData -> Maybe [JData]
getArray (JArray a) = Just a
getArray _          = Nothing
