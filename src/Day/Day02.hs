{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day02 where

import Control.Applicative.Combinators (choice)
import Control.Monad (void)
import Control.Monad.Combinators
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Day ((:~>) (..))
import Parsers (Parser, getExampleInput, getInput)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data OpponentType = A | B | C
  deriving stock (Read, Show)

data ResponseType = X | Y | Z
  deriving stock (Read, Show)

type Line = (OpponentType, ResponseType)

pOpponentType :: Parser OpponentType
pOpponentType =
  choice
    [ A <$ string "A",
      B <$ string "B",
      C <$ string "C"
    ]

pResponseType :: Parser ResponseType
pResponseType =
  choice
    [ X <$ string "X",
      Y <$ string "Y",
      Z <$ string "Z"
    ]

pLine :: Parser Line
pLine = do
  pOp <- pOpponentType
  void $ string " "
  res <- pResponseType
  void $ optional newline
  pure (pOp, res)

pScore :: Line -> Int
pScore = undefined

partA :: [[Int]] -> Int
partA xs = maximum $ map sum xs

partB :: [[Int]] -> Int
partB xs = sum $ take 3 $ reverse $ sort $ map sum xs

main :: IO ()
main = do
  input <- getExampleInput 2 pLine
  print input

-- day02 :: [[Int]] :~> Int
-- day02 = AoC {unParse = parser, unPartA = partA, unPartB = partB}
