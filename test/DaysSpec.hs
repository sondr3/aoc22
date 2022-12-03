module DaysSpec (spec) where

import AoC qualified
import Data.Text (Text)
import Day.Day01 qualified as Day01
import Day.Day02 qualified as Day02
import Day.Day03 qualified as Day03
import GHC.IO (unsafePerformIO)
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

getTestExample :: Int -> Text
getTestExample day = unsafePerformIO $ AoC.rawExampleInput day

getTestInput :: Int -> Text
getTestInput day = unsafePerformIO $ AoC.rawInput day

getTestPExample :: Int -> AoC.Parser a -> [a]
getTestPExample day parser = unsafePerformIO $ AoC.getExampleInput day parser

getTestPInput :: Int -> AoC.Parser a -> [a]
getTestPInput day parser = unsafePerformIO $ AoC.getInput day parser

spec :: Spec
spec = parallel $ do
  describe "Day 01" $ do
    let input = Day01.parser $ getTestInput 1
    let example = Day01.parser $ getTestExample 1
    describe "part 1" $ do
      it "example" $ Day01.partA example `shouldBe` 24000
      it "input" $ Day01.partA input `shouldBe` 68442
    describe "part 2" $ do
      it "example" $ Day01.partB example `shouldBe` 45000
      it "input" $ Day01.partB input `shouldBe` 204837
  describe "Day 02" $ do
    let input = getTestPInput 2 Day02.parser
    let example = getTestPExample 2 Day02.parser
    describe "part 1" $ do
      it "example" $ Day02.partA example `shouldBe` 15
      it "input" $ Day02.partA input `shouldBe` 13924
    describe "part 2" $ do
      it "example" $ Day02.partB example `shouldBe` 12
      it "input" $ Day02.partB input `shouldBe` 13448
  describe "Day 03" $ do
    let input = getTestPInput 3 Day03.parser
    let example = getTestPExample 3 Day03.parser
    describe "part 1" $ do
      it "example" $ Day03.partA example `shouldBe` 157
      it "input" $ Day03.partA input `shouldBe` 7795
    describe "part 2" $ do
      it "example" $ Day03.partB example `shouldBe` 70
      it "example" $ Day03.partB input `shouldBe` 2703
