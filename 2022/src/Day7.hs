module Day7 (solve) where

import Data.Char (isDigit)
import Data.List (foldl', isPrefixOf)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M

type FS = M.Map String Int

parseCmds :: FS -> [String] -> [String] -> FS
parseCmds fs _ [] = fs
parseCmds fs path (cmd : xs) = parseCmds newfs newPath xs
  where
    newPath
      | cmd == "$ cd .." = init path
      | "$ cd" `isPrefixOf` cmd = path ++ [drop 5 cmd]
      | otherwise = path
    newfs
      | isDigit $ head cmd = foldl' (\m k -> M.insertWith (+) k fsize m) fs parents
      | otherwise = fs
      where
        fsize = read $ head $ splitOn " " cmd
        parents = scanl1 (<>) path

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let fs = parseCmds M.empty [] lines
  print $ sum $ M.elems $ M.filter (< 100000) fs
  let toFree = fs M.! "/" - 40000000
  print $ minimum $ M.filter (>= toFree) fs
