module Lib (
    solveDayOne,
    solveDayOnePt2,
    digits,
) where

import Control.Arrow (Arrow ((&&&), (***)), (>>>))
import Data.Char (isDigit, ord)
import Data.List (find, isPrefixOf)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import Data.Ord (clamp)

solveDayOne :: String -> Int
solveDayOne = lines >>> map (filter isDigit >>> (head &&& last) >>> (ord *** ord) >>> (\(x, y) -> x * 10 + y - 528)) >>> sum

lookupMap :: [(String, Int)]
lookupMap =
    [ ("1", 1)
    , ("2", 2)
    , ("3", 3)
    , ("4", 4)
    , ("5", 5)
    , ("6", 6)
    , ("7", 7)
    , ("8", 8)
    , ("9", 9)
    , ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]

digits :: String -> [Int]
digits [] = []
digits xs@(_ : _) =
    let (match, val) = NE.unzip $ find ((`isPrefixOf` xs) . fst) lookupMap
        dropCount = maybe 1 (clamp (1, 6) . subtract 1 . length) match
     in maybeToList val ++ digits (drop dropCount xs)

solveDayOnePt2 :: String -> Int
solveDayOnePt2 = lines >>> map (digits >>> (head &&& last) >>> (\(x, y) -> x * 10 + y)) >>> sum