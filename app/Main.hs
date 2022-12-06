module Main (main) where

import AoC
import Control.Monad (when)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Day.Day02 (day02)
import Day.Day03 (day03)
import Day.Day04 (day04)
import Day.Day05 (day05)
import Day.Day06 (day06)
import System.Environment (getArgs)
import System.Exit (die)

getDay :: Int -> AoC
getDay 1 = day01
getDay 2 = day02
getDay 3 = day03
getDay 4 = day04
getDay 5 = day05
getDay 6 = day06
getDay 7 = undefined
getDay 8 = undefined
getDay 9 = undefined
getDay 10 = undefined
getDay 11 = undefined
getDay 12 = undefined
getDay 13 = undefined
getDay 14 = undefined
getDay 15 = undefined
getDay 16 = undefined
getDay 17 = undefined
getDay 18 = undefined
getDay 19 = undefined
getDay 20 = undefined
getDay 21 = undefined
getDay 22 = undefined
getDay 23 = undefined
getDay 24 = undefined
getDay 25 = undefined
getDay _ = error "No day for that"

runDay :: Int -> Text -> AoC -> IO ()
runDay day input MkAoC {solve} = solve day input

main :: IO ()
main = do
  args <- getArgs
  when (null args) (die "Missing day to run")
  when (length args < 2) (die "Missing file to run")

  let day = read $ head args

  when (day > 25) (die "Cannot run days above 25")
  input <- TIO.readFile $ args !! 1

  runDay day input (getDay day)
