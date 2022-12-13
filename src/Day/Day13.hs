{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day13 where

import Data.List (findIndices, sort)
import Data.List.Split (chunksOf)
import Day (AoC, mkAoC)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Packet = Item Int | List [Packet]
  deriving stock (Show, Eq)

instance Ord Packet where
  compare (Item a) (Item b) = a `compare` b
  compare a@(Item _) b = List [a] `compare` b
  compare a b@(Item _) = a `compare` List [b]
  compare (List a) (List b) = a `compare` b

parser :: Parser [Packet]
parser = packet `sepEndBy` some eol
  where
    packet = choice [Item <$> L.decimal, List <$> between (single '[') (single ']') (packet `sepBy` single ',')]

partA :: [Packet] -> Int
partA xs = sum $ [i | (i, [x, y]) <- zip [1 ..] $ chunksOf 2 xs, x <= y]

partB :: [Packet] -> Int
partB xs = product $ map (+ 1) $ findIndices (\x -> x == d 2 || x == d 6) $ sort $ d 2 : d 6 : xs
  where
    d n = List [List [Item n]]

day13 :: AoC
day13 = mkAoC parser partA partB [13, 6086, 140, 27930]
