{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day03 where

import Control.Monad.Combinators
import Data.List (intersect)
import Data.Maybe (fromJust)
import Parsers (Parser, getInput)
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char

split :: [a] -> ([a], [a])
split xs = splitAt ((length xs + 1) `div` 2) xs

parser :: Parser ([Char], [Char])
parser = split <$> takeWhile1P Nothing (/= '\n') <* optional eol

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

main :: IO ()
main = do
  input <- getInput 3 parser
  print $ partA input
  print $ partB input
