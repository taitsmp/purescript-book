module C5ex where

import Data.Array 

allTrue :: [Boolean] -> Boolean
allTrue [] = true
allTrue (x : xs) = x && allTrue xs

isSorted :: forall a. (Ord a) => [a] -> Boolean
isSorted []  = true
isSorted [_] = true
isSorted (x : y : xs) = x < y && isSorted xs

type Address = { street :: String, city :: String, state :: String } 
type Person  = { first :: String, last :: String, address :: Address }

getCity :: Person -> String
getCity p = p.address.city 


