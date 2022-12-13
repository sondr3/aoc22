{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day05 where

import AoC (AoC, mkAoC)
import Control.Arrow (Arrow ((&&&)))
import Data.Char (isDigit)
import Data.Either (rights)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Parsers (Parser)
import Text.Megaparsec

data Move = Move {cnt :: Int, from :: Int, to :: Int}
  deriving stock (Show)

buildMove :: [Int] -> Move
buildMove (c : f : t : _) = Move {cnt = c, from = f, to = t}
buildMove _ = error "Should not happen"

parser :: Parser (Map Int [Text], [Move])
parser = getInput >>= pure <$> (parseStack &&& parseMoves)
  where
    parseStack xs = Map.fromList $ zip [1 ..] $ map (T.chunksOf 1 . T.init . T.strip) . filter (any isDigit . T.unpack) . T.transpose . takeWhile (not . T.null) $ T.lines xs
    parseMoves xs = map ((((buildMove . map fst) . rights) . map decimal) . T.splitOn " ") (dropWhile (not . ("move" `T.isPrefixOf`)) $ T.lines xs)

partA :: (Map Int [Text], [Move]) -> Text
partA i = T.concat $ map head $ Map.elems (go i)
  where
    go (stacks, []) = stacks
    go (stacks, (Move 0 _ _) : xs) = go (stacks, xs)
    go (stacks, (Move cnt from to) : xs) = go (move stacks from to, Move (cnt - 1) from to : xs)
    move stacks from to = Map.adjust tail from $ Map.adjust (pile :) to stacks
      where
        pile = head $ stacks Map.! from

partB :: (Map Int [Text], [Move]) -> Text
partB i = T.concat $ map head $ Map.elems (go i)
  where
    go (stacks, []) = stacks
    go (stacks, (Move cnt from to) : xs) = go (move stacks cnt from to, xs)
    move stacks cnt from to = Map.adjust (drop cnt) from $ Map.adjust (pile ++) to stacks
      where
        pile = take cnt $ stacks Map.! from

day05 :: AoC
day05 = mkAoC parser partA partB ["CMZ", "RFFFWBPNS", "MCD", "CQQBBJFCS"]
