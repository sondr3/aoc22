{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day.Day07 where

import AoC (AoC, mkAoC, num)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Parsers (Parser)
import Text.Megaparsec hiding (State)

data FS
  = File Int Text
  | Dir Text
  deriving stock (Show, Eq)

data FileTree a = FileTree
  { dirs :: Map Text (FileTree a),
    files :: Map Text a,
    isRoot :: Bool
  }

mkdir :: FileTree a
mkdir = FileTree {dirs = Map.empty, files = Map.empty, isRoot = False}

buildTree :: [[Text]] -> FileTree Int
buildTree fs = fst $ go (tail fs) (mkdir {isRoot = True})
  where
    go (["$", "cd", ".."] : xs) dir = go xs dir
    go (["$", "cd", name] : xs) dir = go xs mkdir
    go (["$", "ls"] : xs) dir = go xs dir
    go (["dir", name] : xs) dir = undefined
    go ([size, name] : xs) dir = undefined
    go [] dir = (dir, [])

parser :: Parser [[Text]]
parser = getInput >>= (pure . map (T.splitOn " ") <$> T.lines)

-- partA :: [[Text]] -> Int
partA xs = buildPaths xs []

-- TODO: rewrite without trying to build the shitty tree, instead do:
-- 1. Keep track of the current directories in a list
-- 2. When a file is found, conat dir list to `/a/b/c/d.dat` with a size
-- 3. Use `isPrefixOf`
-- 4. ???
-- 5. Profit

buildPaths :: [[Text]] -> [Text] -> [(Text, Int)]
buildPaths (["$", "cd", ".."] : xs) fs = buildPaths xs (tail fs)
buildPaths (["$", "cd", dir] : xs) fs = buildPaths xs (dir : fs)
buildPaths (["$", "ls"] : xs) fs = buildPaths xs fs
buildPaths (["dir", _] : xs) fs = buildPaths xs fs
buildPaths ([size, name] : xs) fs = (T.concat (reverse fs) <> "/" <> name, num size) : buildPaths xs fs
buildPaths [] _ = []
buildPaths _ _ = error "you done goofed"

-- traverseDir :: [[Text]] -> [Text] -> FS -> FS
-- traverseDir (["$", "cd", ".."] : xs) cur fs = traverseDir xs (tail cur) fs
-- traverseDir (["$", "cd", dir] : xs) cur fs = traverseDir xs (dir : cur) (Map.insertWith (++) dir [] fs)
-- traverseDir (["$", "ls"] : xs) cur fs = traverseDir xs cur fs
-- traverseDir (["dir", name] : xs) cur fs = traverseDir xs cur (Map.adjust (Dir name :) (head cur) fs)
-- traverseDir ([size, name] : xs) cur fs = traverseDir xs cur (Map.adjust (File (num size) name :) (head cur) fs)
-- traverseDir [] _ fs = fs
-- traverseDir _ _ _ = error "you done goofed"

traverseDir :: [[Text]] -> Text -> Map Text [FS] -> Map Text [FS]
traverseDir (["$", "cd", ".."] : xs) cur fs = traverseDir xs (findDir cur fs) fs
traverseDir (["$", "cd", dir] : xs) _ fs = traverseDir xs dir (Map.insertWith (++) dir [] fs)
traverseDir (["$", "ls"] : xs) cur fs = traverseDir xs cur fs
traverseDir (["dir", name] : xs) cur fs = traverseDir xs cur (Map.adjust (Dir name :) cur fs)
traverseDir ([size, name] : xs) cur fs = traverseDir xs cur (Map.adjust (File (num size) name :) cur fs)
traverseDir [] _ fs = fs
traverseDir _ _ _ = error "you done goofed"

--
-- sumDir :: [FS] -> Int
-- sumDir [] = 0
-- sumDir (Dir _ : xs) = sumDir xs
-- sumDir (File size _ : xs) = size + sumDir xs
--
findDir :: Text -> Map Text [FS] -> Text
findDir val fs = head $ map fst $ filter (\(_, d) -> Dir val `elem` d) (Map.assocs fs)

-- partB :: [[Text]] -> Int
partB xs = undefined

day07 :: AoC
day07 = undefined

-- day07 = mkAoC parser partA partB [95437, 0, 0, 0]
