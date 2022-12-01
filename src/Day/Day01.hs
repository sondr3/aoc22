module Day.Day01 (
    main
) where

import AoC (rawExampleInput, rawInput)
import Data.Text qualified as T
import Data.List (sort)

partA :: [[Int]] -> Int 
partA xs = maximum $ map sum xs

partB :: [[Int]] -> Int 
partB xs = sum $ take 3 $ reverse $ sort $ map sum xs

main :: IO ()
main = do 
  input <- map (map read . words . T.unpack) . T.splitOn "\n\n" <$> rawInput 1
  example <- map (map read . words . T.unpack) . T.splitOn "\n\n" <$> rawExampleInput 1
  print (partA example)
  print (partA input)

  print (partB example)
  print (partB input)
