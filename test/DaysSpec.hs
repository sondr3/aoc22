module DaysSpec (spec) where

import AoC (rawExampleInput, rawInput)
import Data.Text (Text)
import Day.Day01 qualified as Day01
import GHC.IO (unsafePerformIO)
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

getExampleInput :: Int -> Text
getExampleInput day = unsafePerformIO $ rawExampleInput day

getInput :: Int -> Text
getInput day = unsafePerformIO $ rawInput day

spec :: Spec
spec = parallel $ do
  describe "Day 01" $ do
    describe "examples" $ do
      let input = Day01.parser $ getExampleInput 1
      it "part a" $ Day01.partA input `shouldBe` 24000
      it "part b" $ Day01.partB input `shouldBe` 45000
    describe "input" $ do
      let input = Day01.parser $ getInput 1
      it "part a" $ Day01.partA input `shouldBe` 68442
      it "part b" $ Day01.partB input `shouldBe` 204837
