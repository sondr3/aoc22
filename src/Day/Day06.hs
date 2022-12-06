{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day06 where

import AoC (AoC, mkAoC)
import Data.Set qualified as Set
import Data.Text qualified as T
import Parsers (Parser)
import Text.Megaparsec

parser :: Parser String
parser = getInput >>= pure <$> T.unpack

anyEqual :: Ord a => [a] -> Int -> Bool
anyEqual xs n = Set.size (Set.fromList xs) == n

solve :: String -> Int -> Int -> Int
solve xs n res = if anyEqual (take n xs) n then n + res - 1 else solve (tail xs) n (res + 1)

partA :: String -> Int
partA xs = solve xs 4 1

partB :: String -> Int
partB xs = solve xs 14 1

day06 :: AoC
day06 = mkAoC parser partA partB [7, 1134, 19, 2263]
