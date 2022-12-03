{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day03 where

import AoC (AoC, mkAoC)
import Data.List (intersect)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Parsers (Parser)
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char

split :: [a] -> ([a], [a])
split xs = splitAt ((length xs + 1) `div` 2) xs

parser :: Parser [([Char], [Char])]
parser = many $ (split . T.unpack <$> takeWhile1P Nothing (/= '\n')) <* optional eol

scores :: Char -> Int
scores c = fromJust $ lookup c (zip ['a' .. 'z'] [1 ..] ++ zip ['A' .. 'Z'] [27 ..])

findCommon :: ([Char], [Char]) -> Char
findCommon (xs, ys) = head $ xs `intersect` ys

partA :: [([Char], [Char])] -> Int
partA xs = sum $ map (scores . findCommon) xs

partB :: [([Char], [Char])] -> Int
partB xs = sum $ map (scores . head) (threesect $ map (uncurry (++)) xs)
  where
    threesect (x : y : z : ys) = x `intersect` y `intersect` z : threesect ys
    threesect _ = []

day03 :: AoC
day03 = mkAoC parser partA partB [157, 7795, 70, 2703]
