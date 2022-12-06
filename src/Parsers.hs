{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parsers where

import Control.Exception (throw)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec hiding (getInput, parse)
import Text.Megaparsec qualified as M hiding (getInput)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf (printf)

inputName :: Int -> FilePath
inputName = printf "inputs/day%02d.input"

exampleName :: Int -> FilePath
exampleName = printf "inputs/day%02d.test"

getInput :: Int -> Parser a -> IO a
getInput i p = do
  input <- TIO.readFile (inputName i)
  pure $ pLines p input

getExampleInput :: Int -> Parser a -> IO a
getExampleInput i p = do
  input <- TIO.readFile (exampleName i)
  pure $ pLines p input

rawExampleInput :: Int -> IO Text
rawExampleInput day = TIO.readFile (exampleName day)

rawInput :: Int -> IO Text
rawInput day = TIO.readFile (inputName day)

type Parser = Parsec Void Text

number :: Integral a => Parser a
number = L.signed (return ()) L.decimal

pLines :: Parser a -> Text -> a
pLines parser input = case M.parse (many eol *> parser <* many eol) "" input of
  Left err -> throw err
  Right a -> a

getRight :: Either a b -> b
getRight (Right x) = x
getRight _ = error "getRight called with Left value"
