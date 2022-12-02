{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day01 where

import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Day ((:~>) (..))

parser :: Text -> [[Int]]
parser = map (map read . words . T.unpack) . T.splitOn "\n\n"

partA :: [[Int]] -> Int
partA xs = maximum $ map sum xs

partB :: [[Int]] -> Int
partB xs = sum $ take 3 $ reverse $ sort $ map sum xs

day01 :: [[Int]] :~> Int
day01 = AoC {unParse = parser, unPartA = partA, unPartB = partB}
