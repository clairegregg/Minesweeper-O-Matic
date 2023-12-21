module Model
    (
        Square(..), Map, Game, GameState(..), newGame, flagSquare, flipSquare, getTile, getTileSafe,
        Contents(..)
    ) where

import System.Random ( Random(randomRs), StdGen )
import Data.List (nub)

data Contents = Mine | Empty Int deriving (Show, Eq)
data Square = Unflipped Contents | Revealed Contents | Flagged Contents deriving (Show, Eq)
data GameState = Play | Lost | Won deriving (Show, Eq)
type Map = [[Square]]
type Game = (Map, GameState)

{- INTERACTION WITH GAME -}
-- Takes in the map and coordinates of a square, and flags it
flagSquare :: (Int, Int) -> Game ->  Game
flagSquare (x,y) (m,g) = (replaceSquare m (changedSquare (getSquare m (x,y))) (x,y), g)
                         where
                            changedSquare :: Square -> Square
                            changedSquare (Unflipped c) = Flagged c -- If the square is currently unflipped, flag it
                            changedSquare (Flagged c) = Unflipped c -- If the square is currently flagged, unflag it (unflipped)
                            changedSquare (Revealed c) = Revealed c -- If the square has already been flagged, do nothing


-- Flip a set of tiles (if they are flippable).
-- This is done as the set of tiles directly around an Empty 0 tile can always be flipped.
flipAdjacent :: [(Int,Int)] -> Game -> Game
flipAdjacent [] g = g
flipAdjacent ((x,y):cs) (m,g) = if flippable
                                    then flipAdjacent cs (flipSquare (x,y) (m,g))
                                    else flipAdjacent cs (m,g)
                                    where 
                                    sq = getSquareSafe m (x,y)
                                    flippable = case sq of
                                            Just (Unflipped _) -> True
                                            _ -> False

-- Flips a square at the given coordinates. This function also checks the win condition after the square has been flipped.
flipSquare ::  (Int,Int) -> Game -> Game
flipSquare (x,y) (m,g) = (m', checkFlipGameCondition (m',g) (x,y)) -- return the new map (with flipped square), and the new game condition
                        where
                            -- Get the state the square starts in
                            square = getSquare m (x,y)

                            -- Calculate the map with the given square flipped.
                            m' = case square of 
                                -- If the square being flipped is completely empty and not next to any bombs, the flip can expand.
                                Unflipped (Empty 0) -> let 
                                                            -- First, flip the tile itself
                                                            m'' = replaceSquare m (Revealed (Empty 0)) (x,y)
                                                        in
                                                            -- Then, expand the flip in every direction
                                                            fst $ flipAdjacent [(x',y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1]] (m'',g)
                                _ -> replaceSquare m (changedSquare (getSquare m (x,y))) (x,y)

                            -- What should happen to each type of square?
                            changedSquare :: Square -> Square
                            changedSquare (Unflipped c) = Revealed c
                            changedSquare (Flagged c) = Flagged c -- You shouldn't be able to flip a flagged tile
                            changedSquare (Revealed c) = Revealed c

-- Check the game condition when a given tile is flipped.
-- (The win condition check takes in the map with the given tile already flipped).
checkFlipGameCondition :: Game -> (Int,Int) -> GameState
checkFlipGameCondition (m,g) (x,y)
  | checkFlipLoss (getSquareContents m (x,y)) = Lost
  | checkFlipWin m = Won
  | otherwise = g
  where
      -- This function checks if flipping a square will cause an explosion.
      checkFlipLoss :: Contents -> Bool
      checkFlipLoss Mine = True
      checkFlipLoss _ = False

      -- This function checks if (given a certain map), the game has been won.
      -- Note that it does not actually take the flipped tile into account, as that would be less readable.
      checkFlipWin :: Map -> Bool
      checkFlipWin [] = True
      checkFlipWin (r : rs) = checkRowWin r && checkFlipWin rs

      -- Given a row of the map, check if every element in the row is in a correct end-game condition.
      checkRowWin :: [Square] -> Bool
      checkRowWin [] = True
      checkRowWin (s : ss) = checkSquareCorrect s && checkRowWin ss

      -- Check if a square is in a valid win end-game condition.
      checkSquareCorrect :: Square -> Bool
      checkSquareCorrect (Unflipped Mine) = True -- A Mine can either be unflipped or flagged for the game to be won.
      checkSquareCorrect (Flagged Mine) = True
      checkSquareCorrect (Revealed (Empty _)) = True -- An empty tile must be revealed for the game to be won.
      checkSquareCorrect _ = False -- Any other case means the game has not been won yet.

{- HELPER FUNCTIONS -}
getTile :: Game -> (Int,Int) -> Square
getTile (m,_) (x,y) = getSquare m (x,y)

getTileSafe :: Game -> (Int,Int) -> Maybe Square
getTileSafe (m,_) (x,y) = getSquareSafe m (x,y)

-- Get square at coordinates. Not safe!
getSquare :: Map -> (Int, Int) -> Square
getSquare m (x,y) = (m !! y) !! x

-- Alternative to !! which returns Nothing if the element is out of bounds
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
    | n < 0     = Nothing
    | otherwise = safeIndex xs (n - 1)

-- Get square at coordinates -  returns Nothing if coordinates are invalid
getSquareSafe :: Map -> (Int, Int) -> Maybe Square
getSquareSafe m (x,y) = do 
                            r <- m `safeIndex` y
                            r `safeIndex` x

-- Get square contents. Not safe!
getSquareContents :: Map -> (Int, Int) -> Contents
getSquareContents m (x,y) = case getSquare m (x,y) of
                                Unflipped c -> c
                                Revealed c -> c
                                Flagged c -> c

-- Take in a map, a new square, and coordinates, and replace the square at the coordinates with the new square
replaceSquare ::  Map -> Square ->  (Int, Int) -> Map
replaceSquare m s (x,y) = let (preRow, row:postRow) = splitAt y m -- Splits the map into the rows before and after the row containing the intended square
                              (preSq, _:postSq) = splitAt x row -- Splits the row into the squares before and after s, discarding the existing value
                              row' = (preSq ++ [s]) ++ postSq -- Sets the value of row' to include s in the correct location
                          in (preRow ++ [row']) ++ postRow -- Return the map with row' in location

{-- SETUP --}
-- Generate a map of size w * h
emptyMap :: Int -> Int -> Map
emptyMap w h = replicate h (replicate w (Unflipped (Empty 0)))

-- Taking in width, height, and a random generator, generate a list of positions for bombs/mines
bombPositions :: Int -> Int -> [Int] -> [(Int,Int)]
bombPositions w h gen = take (numBombs w h) (indexToXY gen)
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

-- Given a list of coordinates surrounding a bomb, increment the bomb count on each of them.
-- Skip if it's another mine.
updateBombCounts :: Map -> [(Int, Int)] -> Map
updateBombCounts m [] = m
updateBombCounts m (c:cs) = case getSquareContents m c of
                                Mine -> updateBombCounts m cs
                                (Empty x) -> updateBombCounts m' cs
                                    where
                                        m' = replaceSquare m (Unflipped (Empty (x+1))) c

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
                            where m' = replaceSquare m (Unflipped Mine) b
                                  m'' = nextToBomb m' b dimens

-- Generate a new map of the given width and height
newMap :: Int -> Int -> [Int] -> Map
newMap w h g = placeBombs (emptyMap w h) (nub (bombPositions w h g)) (w,h)

-- Generate a new game
newGame :: Int -> Int -> [Int] -> Game
newGame w h g = (newMap w h g, Play)