module Model
    (
        Square, Map, newMap
    ) where

import System.Random ( Random(randomRs), StdGen )

data Square = Mine | Empty Int deriving Show
type Map = [[Square]]

-- Generate a map of size w * h
emptyMap :: Int -> Int -> Map
emptyMap w h = replicate h (replicate w (Empty 0))

-- Taking in width, height, and a random generator, generate a list of positions for bombs/mines
bombPositions :: Int -> Int -> StdGen -> [(Int,Int)]
bombPositions w h gen = take (numBombs w h) (indexToXY (randomRs (0, w*h) gen))
                        where
                            -- randomRS creates a list of indices to the map as a whole
                            -- This function turns these indices into x,y coordinates on the map
                            indexToXY :: [Int] -> [(Int, Int)]
                            indexToXY [] = undefined
                            indexToXY(i:is) = (i `mod` w, i `div` h): indexToXY is

-- Default difficulty is 13% bombs
-- Take in the width and height, and return how many bombs should be placed
numBombs :: Int -> Int -> Int
numBombs w h = floor ((fromIntegral (w*h) * 0.13) :: Double)

-- Get square at coordinates. Not safe!
getSquare :: Map -> (Int, Int) -> Square
getSquare m (x,y) = (m !! y) !! x

-- Take in a map, a new square, and coordinates, and replace the square at the coordinates with the new square
replaceSquare ::  Map -> Square ->  (Int, Int) -> Map
replaceSquare m s (x,y) = let (preRow, row:postRow) = splitAt y m -- Splits the map into the rows before and after the row containing the intended square
                              (preSq, _:postSq) = splitAt x row -- Splits the row into the squares before and after s, discarding the existing value
                              row' = (preSq ++ [s]) ++ postSq -- Sets the value of row' to include s in the correct location
                          in (preRow ++ [row']) ++ postRow -- Return the map with row' in location

-- Given a list of coordinates surrounding a bomb, increment the bomb count on each of them.
-- Skip if it's another mine.
updateBombCounts :: Map -> [(Int, Int)] -> Map
updateBombCounts m [] = m
updateBombCounts m (c:cs) = case getSquare m c of
                                Mine -> updateBombCounts m cs
                                (Empty x) -> updateBombCounts m' cs
                                    where
                                        m' = replaceSquare m (Empty (x+1)) c

-- Given the coordinates of a bomb, increment all valid surrounding coordinates (in all directions, including diagonal)
nextToBomb :: Map -> (Int, Int) -> (Int,Int) -> Map
nextToBomb m (x,y) (w,h) = updateBombCounts m [(x',y') | x' <- xs, y' <- ys]
                           where xs
                                   | x == 0 = [x, x+1]
                                   | x == (w-1) = [x-1, x]
                                   | otherwise = [x-1, x, x+1]
                                 ys
                                   | y == 0 = [y, y+1]
                                   | y == (h-1) = [y-1, y]
                                   | otherwise = [y-1, y, y+1]

-- Taking in a list of bomb coordinates and a map, place the bombs where required, and include neighbouring bomb counts in empty squares
placeBombs :: Map -> [(Int, Int)] -> (Int,Int) -> Map
placeBombs m [] _ = m
placeBombs m (b:bs) dimens = placeBombs m'' bs dimens
                            where m' = replaceSquare m Mine b
                                  m'' = nextToBomb m' b dimens

-- Generate a new map of the given width and height
newMap :: Int -> Int -> StdGen -> Map
newMap w h g = placeBombs (emptyMap w h) (bombPositions w h g) (w,h)