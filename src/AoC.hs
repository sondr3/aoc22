{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module AoC where

import Text.Printf (printf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (throw)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

inputName :: Int -> FilePath
inputName = printf "inputs/day%02d.input" 

exampleName :: Int -> FilePath
exampleName = printf "inputs/day%02d.test" 

getInput :: Int -> Parser a -> IO [a]
getInput i p = do
  input <- readFile (inputName i)
  pure $ pLines p input

getExampleInput :: Int -> Parser a -> IO [a]
getExampleInput i p = do
  input <- readFile (exampleName i)
  pure $ pLines p input

rawExampleInput :: Int -> IO Text 
rawExampleInput day = T.strip <$> TIO.readFile (exampleName day)

rawInput :: Int -> IO Text 
rawInput day = T.strip <$> TIO.readFile (inputName day)

type Parser = Parsec Void String

number :: Integral a => Parser a
number = L.signed (return ()) L.decimal

pLines :: Parser a -> String -> [a]
pLines parser input = case parse (traverse p (lines input)) "" input of
  Left err -> throw err
  Right a -> a
  where
    p l = setInput l *> parser <* eof <* setInput "\n" <* newline

someFunc :: IO ()
someFunc = putStrLn "someFunc"
