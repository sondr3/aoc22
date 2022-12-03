{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day where

import Formatting (formatToString)
import Formatting.Clock (timeSpecs)
import Parsers (Parser, getInput)
import System.Clock (Clock (Monotonic), getTime)
import Text.Printf (printf)

data AoC = forall i o.
  (Eq o, Show o) =>
  MkAoC
  { parse :: Parser i,
    part1 :: i -> o,
    part2 :: i -> o,
    answers :: [o],
    solve :: Int -> IO ()
  }

mkAoC :: (Eq o, Show o) => Parser i -> (i -> o) -> (i -> o) -> [o] -> AoC
mkAoC p p1 p2 ans =
  MkAoC
    { parse = p,
      part1 = p1,
      part2 = p2,
      answers = ans,
      solve = \day -> do
        input <- getInput day p

        p1_time <- getTime Monotonic
        let p1_res = p1 input
        p1_elapsed <- getTime Monotonic

        p2_time <- getTime Monotonic
        let p2_res = p2 input
        p2_elapsed <- getTime Monotonic

        putStrLn $ printf "Solution for day %02d" day
        putStrLn $ "  Part 1: " <> show p1_res <> " in " <> formatToString timeSpecs p1_time p1_elapsed
        putStrLn $ "  Part 2: " <> show p2_res <> " in " <> formatToString timeSpecs p2_time p2_elapsed
    }

runDay :: Int -> AoC -> IO ()
runDay day MkAoC {..} = solve day
