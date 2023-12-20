module MinesweeperElements
    (
        flaggedTile, unflippedTile, bombTile
        , bird0, bird1, bird2, bird3, bird4
        , bird5, bird6, bird7, bird8
        , startMap
        ) 
where

{-# LANGUAGE OverloadedStrings #-}

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as U

startMap :: (Int,Int) -> U.UI UI.Element
startMap (w,h) = UI.div UI.#+ [UI.grid (replicate h (replicate w unflippedTile))]
                        UI.# UI.set (UI.attr "class") "grid"

flaggedTile :: U.UI UI.Element
flaggedTile = UI.img UI.# UI.set UI.src "./static/Flagged Egg.png"
                            UI.# UI.set UI.alt "Flagged tile"
                            UI.# UI.set (UI.attr "class") "tile"

unflippedTile :: U.UI UI.Element
unflippedTile = UI.img UI.# UI.set UI.src "./static/Egg.png" 
                            UI.# UI.set UI.alt "Unflipped tile, we don't know what's behind it!"
                            UI.# UI.set (UI.attr "class") "tile"

bombTile :: U.UI UI.Element
bombTile = UI.img UI.# UI.set UI.src "./static/Bomb.png" 
                            UI.# UI.set UI.alt "Bomb"
                            UI.# UI.set (UI.attr "class") "tile"

bird0 :: U.UI UI.Element
bird0 = UI.img UI.# UI.set UI.src "./static/Bird 0.png" 
                            UI.# UI.set UI.alt "This tile has no bombs next to it."
                            UI.# UI.set (UI.attr "class") "tile"

bird1 :: U.UI UI.Element
bird1 = UI.img UI.# UI.set UI.src "./static/Bird 1.png" 
                            UI.# UI.set UI.alt "This tile has 1 bomb next to it."
                            UI.# UI.set (UI.attr "class") "tile"

bird2 :: U.UI UI.Element
bird2 = UI.img UI.# UI.set UI.src "./static/Bird 2.png" 
                            UI.# UI.set UI.alt "This tile has 2 bombs next to it."
                            UI.# UI.set (UI.attr "class") "tile"

bird3 :: U.UI UI.Element
bird3 = UI.img UI.# UI.set UI.src "./static/Bird 3.png" 
                            UI.# UI.set UI.alt "This tile has 3 bombs next to it."
                            UI.# UI.set (UI.attr "class") "tile"

bird4 :: U.UI UI.Element
bird4 = UI.img UI.# UI.set UI.src "./static/Bird 4.png" 
                            UI.# UI.set UI.alt "This tile has 4 bombs next to it."
                            UI.# UI.set (UI.attr "class") "tile"

bird5 :: U.UI UI.Element
bird5 = UI.img UI.# UI.set UI.src "./static/Bird 5.png" 
                            UI.# UI.set UI.alt "This tile has 5 bombs next to it."
                            UI.# UI.set (UI.attr "class") "tile"

bird6 :: U.UI UI.Element
bird6 = UI.img UI.# UI.set UI.src "./static/Bird 6.png" 
                            UI.# UI.set UI.alt "This tile has 6 bombs next to it."
                            UI.# UI.set (UI.attr "class") "tile"

bird7 :: U.UI UI.Element
bird7 = UI.img UI.# UI.set UI.src "./static/Bird 7.png" 
                            UI.# UI.set UI.alt "This tile has 7 bombs next to it."
                            UI.# UI.set (UI.attr "class") "tile"

bird8 :: U.UI UI.Element
bird8 = UI.img UI.# UI.set UI.src "./static/Bird 8.png" 
                            UI.# UI.set UI.alt "This tile has 8 bombs next to it."
                            UI.# UI.set (UI.attr "class") "tile"