{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day01 where

import Data.Char (isDigit)
import Data.Either (rights)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Day (AoC, mkAoC)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

parser :: Text -> [[Int]]
parser = map (map read . words . T.unpack) . T.splitOn "\n\n"

parser' :: Parser [[Int]]
parser' = do
  bags <- head <$> (group `sepBy` eol)
  pure $ map ((map fst . rights) . map decimal) bags
  where
    group = dbg "group" $ some num `sepBy` eol
    num = dbg "num" $ takeWhile1P Nothing isDigit <* eol

partA :: [[Int]] -> Int
partA xs = maximum $ map sum xs

partB :: [[Int]] -> Int
partB xs = sum $ take 3 $ reverse $ sort $ map sum xs

day01 :: AoC
day01 = mkAoC parser' partA partB [24000, 68442, 45000, 204837]
