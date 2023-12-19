module Main (main) where


import Model
import System.Random
import Model (flipSquare)

main :: IO ()
main = print g
       where 
        g = flipSquare (2,2) (flipSquare (2,1) (flipSquare (2,0) (flipSquare (1,2) (flipSquare (1,1) (flipSquare (1,0) (flipSquare (0,2) (flipSquare (0,1) (newGame 3 3 (mkStdGen 42)))))))))

