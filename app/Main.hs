module Main (main) where

import Lib

main :: IO ()
main = do
    doc <- readFile "1.txt" 
    print $ solveDayOne doc 
