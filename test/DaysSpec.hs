module DaysSpec (spec) where

import AoC (AoC (..), Parser, getExampleInput, getInput)
import Day.Day01 (day01)
import Day.Day02 (day02)
import Day.Day03 (day03)
import Day.Day04 (day04)
import Day.Day05 (day05)
import Day.Day06 (day06)
import Day.Day07 (day07)
import GHC.IO (unsafePerformIO)
import Test.Hspec (Spec, describe, it, parallel, shouldBe)
import Text.Printf (printf)

getTestPExample :: Int -> Parser a -> a
getTestPExample day parser = unsafePerformIO $ getExampleInput day parser

getTestPInput :: Int -> Parser a -> a
getTestPInput day parser = unsafePerformIO $ getInput day parser

testDay :: Int -> AoC.AoC -> Spec
testDay day MkAoC {parse, part1, part2, answers} = describe (printf "Day %02d" day) $ do
  let input = getTestPInput day parse
      example = getTestPExample day parse
  describe "part 1" $ do
    it "example" $ part1 example `shouldBe` head answers
    it "input" $ part1 input `shouldBe` answers !! 1
  describe "part 2" $ do
    it "example" $ part2 example `shouldBe` answers !! 2
    it "input" $ part2 input `shouldBe` answers !! 3

spec :: Spec
spec = parallel $ do
  testDay 1 day01
  testDay 2 day02
  testDay 3 day03
  testDay 4 day04
  testDay 5 day05
  testDay 6 day06
  testDay 7 day07
