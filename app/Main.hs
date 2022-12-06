module Main (main) where

import AoC
import Control.Monad (when)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Day.Day02 (day02)
import Day.Day03 (day03)
import Day.Day04 (day04)
import Day.Day05 (day05)
import System.Environment (getArgs)
import System.Exit (die)

getDay :: Int -> AoC
getDay 1 = day01
getDay 2 = day02
getDay 3 = day03
getDay 4 = day04
getDay 5 = day05
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
