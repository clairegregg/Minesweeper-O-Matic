module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import Model
import System.Random
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as U
import MinesweeperElements
import Data.IORef (newIORef)
import Control.Monad.IO.Class (MonadIO(liftIO))


{-g = flipSquare (2,2) (flipSquare (2,1) (flipSquare (2,0) (flipSquare (1,2) (flipSquare (1,1) (flipSquare (1,0) (flipSquare (0,2) (flipSquare (0,1) (newGame 3 3 (mkStdGen 42)))))))))-}

main :: IO ()
main = U.startGUI U.defaultConfig
       { U.jsPort = Just 8023
       , U.jsStatic = Just "./static/"} 
       setup

setup :: U.Window -> U.UI ()
setup window = do
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
    button <- UI.button U.# U.set U.text "Click me!"

    game <- liftIO $ newIORef $ newGame 10 10 (mkStdGen 42)


    -- Define UI layout
    _ <- U.getBody window U.#+ [UI.element title, startMap (10,10) game, U.element description]

    -- Define event handling
    U.on UI.click button $ \_ -> do
        U.element button U.# U.set U.text "Hello world!"

