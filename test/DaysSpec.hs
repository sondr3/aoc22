module DaysSpec (spec) where

import AoC qualified
import Data.Text (Text)
import Day.Day01 qualified as Day01
import Day.Day02 qualified as Day02
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
    describe "examples" $ do
      let input = Day01.parser $ getTestExample 1
      it "part a" $ Day01.partA input `shouldBe` 24000
      it "part b" $ Day01.partB input `shouldBe` 45000
    describe "input" $ do
      let input = Day01.parser $ getTestInput 1
      it "part a" $ Day01.partA input `shouldBe` 68442
      it "part b" $ Day01.partB input `shouldBe` 204837
  describe "Day 02" $ do
    describe "examples" $ do
      let input = getTestPExample 2 Day02.parser
      it "part a" $ Day02.partA input `shouldBe` 15
      it "part b" $ Day02.partB input `shouldBe` 12
    describe "input" $ do
      let input = getTestPInput 2 Day02.parser
      it "part a" $ Day02.partA input `shouldBe` 13924
      it "part b" $ Day02.partB input `shouldBe` 13448
