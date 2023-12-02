module Main (main) where

import Data.List (intercalate)
import Lib

main :: IO ()
main = do
    doc <- readFile "1.txt"
    print $ solveDayOne doc
    print $ solveDayOnePt2 doc