module Day8 where

import Data.List (foldl1')
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Text.Regex.TDFA (getAllTextMatches, (=~))

type Map = M.Map String (String, String)

parse :: [String] -> (String, Map)
parse lines = (cycle $ head lines, nodes)
  where
    nodes = M.fromList $ (\[a, b, c] -> (a, (b, c))) . parseL <$> drop 2 lines
    parseL line = getAllTextMatches $ line =~ ("([A-Z0-9]){3}" :: String) :: [String]

move :: Map -> Char -> String -> String
move map dir loc = if dir == 'L' then a else b
  where
    (a, b) = fromJust $ M.lookup loc map

findPath :: Map -> (String -> Bool) -> String -> String -> Int
findPath map check (p : path) loc
  | check next = 1
  | otherwise = 1 + findPath map check path next
  where
    next = move map p loc

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let (path, map_) = parse input

  print $ findPath map_ (== "ZZZ") path "AAA"

  let starts = filter ((== 'A') . last) $ M.keys map_
  print $ foldl1' lcm $ findPath map_ ((== 'Z') . last) path <$> starts
