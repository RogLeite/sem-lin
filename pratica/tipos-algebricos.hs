data Bill = T | F
--            deriving (Show, Eq)
--{-
instance Show Bill where
    tostring T  = "T"
    tostring F  = "F"
{-
instance (Show a) => Show (Maybe a) where
    toString Nothing  = "Nothing"
    toString Just a = "Just " ++ toString a
    
-- -}-- -}

not:: Bill -> Bill
not b = case b of 
        F -> T
        T -> F


ou:: Bill -> Bill -> Bill
ou T _ = T
ou F y = y

