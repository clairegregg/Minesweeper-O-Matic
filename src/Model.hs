module Model 
    (
        Square, Map, emptyMap
    ) where

data Square = Mine | Empty Int deriving Show
type Map = [[Square]]

emptyMap :: Int -> Int -> Map
emptyMap w h = take h $ repeat $ take w $ repeat (Empty 0)