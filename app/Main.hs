module Main (main) where


import Model
import System.Random

main :: IO ()
main = print m
       where (m,_) = flagSquare (newGame 10 10 (mkStdGen 42)) (0,0)
