module Main (main) where


import Model
import System.Random

main :: IO ()
main = print $ newMap 10 10 (mkStdGen 42)
