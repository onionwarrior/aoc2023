module Main (main) where

import Data.List (intercalate)
import Lib

main :: IO ()
main = do
    doc <- readFile "1.txt"
    doc2 <- readFile "2.txt"
    print $ solveDayOne doc
    print $ solveDayOnePt2 doc
    print $ solveDayTwo doc2
    print $ solveDayTwoPt2 doc2