module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

--import Model
--import System.Random
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as U

{-g = flipSquare (2,2) (flipSquare (2,1) (flipSquare (2,0) (flipSquare (1,2) (flipSquare (1,1) (flipSquare (1,0) (flipSquare (0,2) (flipSquare (0,1) (newGame 3 3 (mkStdGen 42)))))))))-}

main :: IO ()
main = U.startGUI U.defaultConfig
       { U.jsPort = Just 8023
       , U.jsStatic = Just "./static/"} 
       setup

setup :: U.Window -> U.UI ()
setup window = do
    -- Create UI elements
    button <- UI.button U.# U.set U.text "Click me!"
    image <- emptyTile

    -- Define UI layout
    _ <- U.getBody window U.#+ [U.element button, U.element image]

    -- Define event handling
    U.on UI.click button $ \_ -> do
        U.element button U.# U.set U.text "Hello world!"


{-createMap :: (Int,Int) -> UI.Element
createMap (w,h) = -}

emptyTile :: U.UI U.Element
emptyTile = UI.img UI.# UI.set UI.src "./static/Egg.png"