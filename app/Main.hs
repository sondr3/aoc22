module Main where

import AoC qualified (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  AoC.someFunc
