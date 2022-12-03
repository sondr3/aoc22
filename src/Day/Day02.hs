{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day02 where

import AoC (AoC, mkAoC)
import Control.Applicative.Combinators
import Control.Monad (void)
import Data.Foldable (foldl')
import Parsers (Parser)
import Text.Megaparsec.Char (char, newline, string)

pScore :: (Char, Char) -> Int
pScore ('A', 'X') = 1 + 3
pScore ('A', 'Y') = 2 + 6
pScore ('A', 'Z') = 3 + 0
pScore ('B', 'X') = 1 + 0
pScore ('B', 'Y') = 2 + 3
pScore ('B', 'Z') = 3 + 6
pScore ('C', 'X') = 1 + 6
pScore ('C', 'Y') = 2 + 0
pScore ('C', 'Z') = 3 + 3
pScore _ = undefined

pEnd :: (Char, Char) -> Int
pEnd ('A', 'X') = 3 + 0
pEnd ('A', 'Y') = 1 + 3
pEnd ('A', 'Z') = 2 + 6
pEnd ('B', 'X') = 1 + 0
pEnd ('B', 'Y') = 2 + 3
pEnd ('B', 'Z') = 3 + 6
pEnd ('C', 'X') = 2 + 0
pEnd ('C', 'Y') = 3 + 3
pEnd ('C', 'Z') = 1 + 6
pEnd _ = undefined

parser :: Parser [(Char, Char)]
parser = many $ do
  x <- choice [char 'A', char 'B', char 'C']
  void $ string " "
  y <- choice [char 'X', char 'Y', char 'Z']
  void $ optional newline
  pure (x, y)

solve :: ((Char, Char) -> Int) -> [(Char, Char)] -> Int
solve f = foldl' (\acc x -> f x + acc) 0

partA :: [(Char, Char)] -> Int
partA = solve pScore

partB :: [(Char, Char)] -> Int
partB = solve pEnd

day02 :: AoC
day02 = mkAoC parser partA partB [15, 13924, 12, 13448]
