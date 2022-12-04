{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day04 where

import AoC (AoC, getRight, mkAoC)
import Control.Monad (void)
import Data.Char (isDigit)
import Data.List (intersect)
import Data.Text.Read (decimal)
import Parsers (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char

parser :: Parser [[(Int, Int)]]
parser = some items
  where
    items = item `sepBy1` string "," <* (eol <|> eof *> "")
    item = do
      num1 <- (fst <$> getRight) . decimal <$> takeWhile1P Nothing isDigit
      void $ string "-"
      num2 <- (fst <$> getRight) . decimal <$> takeWhile1P Nothing isDigit
      pure (num1, num2)

numContained :: (Int, Int) -> (Int, Int) -> Bool
numContained xs ys = comp xs ys || comp ys xs
  where
    comp (a, b) (x, y) = [a .. b] `intersect` [x .. y] == [a .. b]

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap xs ys = comp xs ys || comp ys xs
  where
    comp (a, b) (x, y) = not . null $ [a .. b] `intersect` [x .. y]

take2 :: [a] -> (a -> a -> b) -> b
take2 (a : b : _) f = f a b
take2 _ _ = undefined

partA :: [[(Int, Int)]] -> Int
partA xs = sum $ map (fromEnum . (`take2` numContained)) xs

partB :: [[(Int, Int)]] -> Int
partB xs = sum $ map (fromEnum . (`take2` overlap)) xs

day04 :: AoC
day04 = mkAoC parser partA partB [2, 433, 4, 852]
