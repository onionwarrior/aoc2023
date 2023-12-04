{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use all" #-}

module Lib (
    solveDayOne,
    solveDayOnePt2,
    digits,
    solveDayTwo,
    solveDayTwoPt2,
) where

import Control.Arrow (Arrow ((&&&), (***)), (>>>))
import Control.Lens
import Control.Monad.State (State, execState, get, put)
import Data.Char (isDigit, ord)
import Data.Functor (($>))
import Data.List (find, isPrefixOf)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Ord (clamp)
import Text.Parsec (ParsecT, parse, sepBy, (<|>))
import Text.Parsec.Number (nat)
import Text.ParserCombinators.Parsec.Char

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

data Color = R | G | B
instance Show Color where
    show = \case
        R -> "red"
        G -> "green"
        B -> "blue"
data Reveal = Reveal {_count :: Int, _color :: Color} deriving (Show)

game :: ParsecT String u Identity (Int, [[Reveal]])
game = do
    _ <- string "Game"
    _ <- space
    id' <- nat @Int
    _ <- char ':'
    pulls <- pull `sepBy` char ';'
    return (id', pulls)

pull :: ParsecT String u Identity [Reveal]
pull = space >> reveal `sepBy` char ','

reveal :: ParsecT String u Identity Reveal
reveal = do
    spaces
    count' <- nat
    spaces
    color' <-
        string "green"
            $> G
            <|> string "red"
            $> R
            <|> string "blue"
            $> B
    return $ Reveal count' color'

solveDayTwo :: [Char] -> Int
solveDayTwo s =
    let games = lines s
        rightToMaybe = either (const Nothing) Just
        isValidReveal (Reveal count' color') = case color' of
            R -> count' <= 12
            G -> count' <= 13
            B -> count' <= 14
        games' = mapMaybe (rightToMaybe . parse game "") games
     in sum . map fst . filter (all isValidReveal . concat . snd) $ games'

data CubeSet = CubeSet {_red :: Int, _green :: Int, _blue :: Int}

solveDayTwoPt2 :: String -> Int
solveDayTwoPt2 s =
    let rightToMaybe = either (const Nothing) Just
        games = lines >>> mapMaybe (rightToMaybe . parse game "") >>> map (snd >>> concat) $ s
        update :: Reveal -> State CubeSet Reveal
        update reveal'@(Reveal count' color') = do
            CubeSet red' green' blue' <- get
            put $ case color' of
                R -> CubeSet (max count' red') green' blue'
                G -> CubeSet red' (max count' green') blue'
                B -> CubeSet red' green' (max count' blue')
            return reveal'
        sumForGame game' = r * g * b
          where
            CubeSet r g b = execState (traverse update game') $ CubeSet 0 0 0
     in sum . map sumForGame $ games