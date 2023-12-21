module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import Model
import System.Random
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as U
import MinesweeperElements ( endGameCover, startMap, Tile )
import Data.IORef (newIORef)
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.IORef (IORef(IORef))

main :: IO ()
main = U.startGUI U.defaultConfig
       { U.jsPort = Just 8023
       , U.jsStatic = Just "./static/"} 
       setup

setup :: U.Window -> U.UI ()
setup window = do
    U.runFunction $ U.ffi "document.oncontextmenu = function(e) { e.preventDefault(); };"
    UI.addStyleSheet window "styles.css"

    -- Create UI elements
    title <- UI.h1 U.# U.set U.text "Bird Minesweeper"
    description <- UI.div U.#+ [
        UI.h2 U.# U.set U.text "Welcome to Bird Minesweeper!", 
        UI.p U.# U.set U.text "To win, you must show every bird on the grid by opening their egg, but do not break open any eggs with bombs inside!", 
        UI.br,
        UI.h2 U.# U.set U.text "Instructions", 
        UI.p U.# U.set U.text "Left click on an egg to reveal what's inside.",
        UI.br,
        UI.p U.# U.set U.text "Right click on an egg to 'flag' it if you think there's a bomb inside."
        ]
        UI.# UI.set (UI.attr "class") "description"
    cover <- endGameCover

    -- Create the model of the game
    game <- liftIO $ newIORef $ newGame 10 10 (mkStdGen 42)

    -- Define UI layout
    _ <- U.getBody window U.#+ [UI.element title, startMap (10,10) game (UI.element cover), U.element description]

    return ()


