module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import Model
import System.Random
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as U
import MinesweeperElements ( endGameCover, startMap, playButton )
import Data.IORef (newIORef, writeIORef)
import Control.Monad.IO.Class (MonadIO(liftIO))

width :: Int
width = 10

height :: Int
height = 10

difficulty :: Double
difficulty = 0.13

main :: IO ()
main = U.startGUI U.defaultConfig
       { U.jsPort = Just 8023
       , U.jsStatic = Just "./static/"} -- This allows images to be used
       setup

setup :: U.Window -> U.UI ()
setup window = do
    U.runFunction $ U.ffi "document.oncontextmenu = function(e) { e.preventDefault(); };"
    UI.addStyleSheet window "styles.css"

    -- Create the model of the game
    gen <- newStdGen
    let randomStream = randomRs (0, width*height) gen
    game <- liftIO $ newIORef $ newGame width height randomStream difficulty

    -- Create UI elements
    title <- UI.h1 U.# U.set U.text "Bird Minesweeper"
    description <- UI.div U.#+ [
        UI.h2 U.# U.set U.text "Welcome to Bird Minesweeper!",
        UI.p U.# U.set U.text ("To win, you must show every bird on the grid by opening their egg, but do not break open any eggs with bombs inside! There are "++ show (floor (fromIntegral (width*height) * difficulty) :: Integer) ++ " bombs on the map."),
        UI.br,
        UI.h2 U.# U.set U.text "Instructions",
        UI.p U.# U.set U.text "Left click on an egg to reveal what's inside.",
        UI.br,
        UI.p U.# U.set U.text "Right click on an egg to 'flag' it if you think there's a bomb inside.",
        UI.br,
        UI.p U.# U.set U.text "If you get stuck, press the play button. If there is a correct move, it will make it for you. If there is not a correct move, it will choose a random tile to flip. Note that the Play button only works if all your flags are in correct positions!"
        ]
        UI.# UI.set (UI.attr "class") "description"
    cover <- endGameCover
    m <- startMap (width,height) game (UI.element cover)
    button <- playButton (U.element m) game
    reset <- UI.button UI.# U.set U.text "Reset"
                            UI.#. "button"
    container <- UI.div UI.#+ [UI.div UI.#+ [UI.element button, UI.element reset]
                                        UI.#. "button-div"]
                           UI.#. "container"

    -- Define UI layout
    _ <- U.getBody window U.#+ [UI.element title, U.element m, U.element container, U.element description]

    -- Set behaviour for reset button to reset the shared game
    U.on UI.click reset $ \_ -> do
        gen' <- newStdGen
        let randomStream' = randomRs (0, 10*10) gen'
        liftIO $ writeIORef game (newGame 10 10 randomStream' difficulty)
        U.runFunction $ U.ffi "document.getElementById('grid').click();"


