module MinesweeperElements
    (
        startTile, startMap, MapVisuals, endGameCover, Tile
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

startMap :: (Int,Int) -> IORef M.Game ->  U.UI UI.Element -> MapVisuals
startMap (w,h) g  cover = do
                            let tiles = makeMap (w,h) 0 g
                            grid <- UI.div UI.#+ [UI.grid tiles,cover]
                                    UI.# UI.set (UI.attr "class") "grid"
                            
                            U.on UI.click grid $ \_ -> do
                                game <- liftIO $ readIORef g
                                _ <- changeEndGameCover game cover
                                let newTiles = updateGrid 0 game tiles
                                U.element grid UI.# UI.set U.children [] 
                                     UI.#+ [UI.grid newTiles,cover]    
                            UI.element grid


makeMap :: (Int,Int) -> Int -> IORef M.Game -> [[Tile]]
makeMap (w,h) y g = if y >= h then [] else makeRow w 0 y g  : makeMap (w,h) (y+1) g

makeRow :: Int -> Int -> Int -> IORef M.Game ->  [Tile]
makeRow w x y g  = if x >= w then [] else startTile (x,y) g : makeRow w (x+1) y g

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
                                    let (map', game_s') = M.flipSquare (x,y) game
                                    liftIO $ writeIORef g (map', game_s')

                            U.element tile

updateTile :: M.Square -> Tile -> Tile
updateTile square tile = do
                            let (image, alt) = tileTypeToDisplay square
                            tile UI.# U.set UI.src image
                                UI.# U.set UI.alt alt


updateRow :: (Int, Int) -> M.Game ->  [Tile] -> [Tile]
updateRow _ _  [] = []
updateRow (x,y) game  (t:ts) = case M.getTileSafe game (x, y) of
                                Nothing -> []
                                Just sq -> do
                                            tile <- [updateTile sq t]
                                            tile : updateRow (x+1,y) game ts


updateGrid :: Int -> M.Game -> [[Tile]] -> [[Tile]]
updateGrid _ _  [] = []
updateGrid y game  (ts:tss) = do
                                row <- [updateRow (0,y) game ts]
                                row : updateGrid (y+1) game tss

endGameCover :: U.UI UI.Element
endGameCover =  UI.div UI.#. "grid-cover invisible-grid"

changeEndGameCover :: M.Game -> U.UI UI.Element -> U.UI UI.Element
changeEndGameCover (_,g) cover
  | g == M.Play = cover UI.#. "grid-cover invisible-grid"
               UI.# UI.set U.children []
  | g == M.Won = cover UI.#. "grid-cover shown-grid"
           UI.#+ [wonDisplay]
  | otherwise = cover UI.#. "grid-cover shown-grid"
           UI.#+ [lostDisplay]

lostDisplay :: U.UI UI.Element
lostDisplay = do
                image <- UI.img UI.# UI.set UI.src "./static/Bomb.png"
                                UI.# UI.set UI.alt "Bomb!"
                                UI.#. "result-img"
                text <- UI.p UI.# UI.set UI.text "You blew up!"
                                UI.#. "result-text"
                contents <- UI.div UI.#+ [UI.element image, UI.element text]
                                UI.#. "result-div"
                UI.element contents

wonDisplay :: U.UI UI.Element
wonDisplay = do
                image <- UI.img UI.# UI.set UI.src "./static/Bird 0.png"
                                UI.# UI.set UI.alt "A revealed bird"
                                UI.#. "result-img"
                text <- UI.p UI.# UI.set UI.text "You win - you revealed all of the birds!"
                                UI.#. "result-text"
                contents <- UI.div UI.#+ [UI.element image, UI.element text]
                                UI.#. "result-div"
                UI.element contents
