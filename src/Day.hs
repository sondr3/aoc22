{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day where

import Data.Text (Text)
import Parsers (rawInput)
import Text.Printf (printf)

data a :~> b = AoC
  { unParse :: Text -> a,
    unPartA :: a -> b,
    unPartB :: a -> b
  }

runDay :: Show b => Int -> a :~> b -> IO ()
runDay day AoC {..} = do
  input <- unParse <$> rawInput day
  let a = unPartA input
      b = unPartB input

  putStrLn $ printf "Day %02d solutions" day
  putStrLn $ "solution A: " <> show a
  putStrLn $ "solution B: " <> show b
