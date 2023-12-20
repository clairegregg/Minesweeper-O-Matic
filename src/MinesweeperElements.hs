module MinesweeperElements
    (
        flaggedTile, startTile
        , startMap, MapVisuals
        ) 
where

{-# LANGUAGE OverloadedStrings #-}

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as U
import qualified Model as M
import Data.IORef (IORef, readIORef, writeIORef)
import Control.Monad.IO.Class (MonadIO(liftIO))

type MapVisuals = (U.UI UI.Element)
type Tile = (U.UI UI.Element)

startMap :: (Int,Int) -> IORef M.Game -> MapVisuals
startMap (w,h) g = UI.div UI.#+ [UI.grid (makeMap (w,h) 0 g)]
                        UI.# UI.set (UI.attr "class") "grid" 

makeMap :: (Int,Int) -> Int -> IORef M.Game -> [[Tile]]
makeMap (w,h) y g = if y >= h then [] else makeRow w 0 y g : makeMap (w,h) (y+1) g

makeRow :: Int -> Int -> Int -> IORef M.Game -> [Tile]
makeRow w x y g = if x >= w then [] else startTile (x,y) g : makeRow w (x+1) y g

flaggedTile :: (Int, Int) -> IORef M.Game -> U.UI UI.Element
flaggedTile (x,y) g = do 
                        tile <- UI.img UI.# UI.set UI.src "./static/Flagged Egg.png"
                                    UI.# UI.set UI.alt "Flagged tile"
                                    UI.# UI.set (UI.attr "class") "tile"
                        U.element tile

tileTypeToDisplay :: M.Square -> (String, String)
tileTypeToDisplay (M.Unflipped _) = ("./static/Egg.png", "Unflipped tile, we don't know what's behind it!")
tileTypeToDisplay (M.Flagged _) = ("./static/Flagged Egg.png", "Flagged tile")
tileTypeToDisplay (M.Revealed M.Mine) = ("./static/Bomb.png", "Bomb")
tileTypeToDisplay (M.Revealed (M.Empty x)) = emptyTileType x

emptyTileType :: Int -> (String, String)
emptyTileType x
    | x == 0 = ("./static/Bird 0.png", "This tile has no bombs next to it.")
    | x == 1 = ("./static/Bird 1.png", "This tile has no bombs next to it.")
    | x == 2 = ("./static/Bird 2.png", "This tile has 2 bombs next to it.")
    | x == 3 = ("./static/Bird 3.png", "This tile has 3 bombs next to it.")
    | x == 4 = ("./static/Bird 4.png", "This tile has 4 bombs next to it.")
    | x == 5 = ("./static/Bird 5.png", "This tile has 5 bombs next to it.")
    | x == 6 = ("./static/Bird 6.png", "This tile has 6 bombs next to it.")
    | x == 7 = ("./static/Bird 7.png", "This tile has 7 bombs next to it.")
    | x == 8 = ("./static/Bird 8.png", "This tile has 8 bombs next to it.")
    | otherwise = ("./static/Bird 8.png", "This tile has 8 bombs next to it.")


startTile :: (Int, Int) -> IORef M.Game -> U.UI UI.Element
startTile (x,y) g = do 
                            tile <- UI.img UI.# UI.set UI.src "./static/Egg.png" 
                                    UI.# UI.set UI.alt "Unflipped tile, we don't know what's behind it!"
                                    UI.# UI.set (UI.attr "class") "tile"
                                    
                            U.on UI.click tile $ \_ -> do
                                    game <- liftIO $ readIORef g 
                                    let game' = M.flipSquare (x,y) game
                                    liftIO $ writeIORef g game'
                                    let square = M.getTile game' (x,y)
                                    let (image, alt) = tileTypeToDisplay square
                                    U.element tile UI.# U.set UI.src image
                                                    UI.# U.set UI.alt alt

                            U.element tile