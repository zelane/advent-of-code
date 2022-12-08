module Day2b (solve) where

import Data.List (elemIndex, isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

toPair :: String -> (Char, Char)
toPair s = (rps a, rps b)
  where
    [[a], [b]] = splitOn " " s

    rps x
      | x `elem` ['A', 'X'] = 'R'
      | x `elem` ['B', 'Y'] = 'P'
      | x `elem` ['C', 'Z'] = 'S'

beats :: [Char]
beats = ['R', 'P', 'S', 'R']

score :: Char -> Int
score x = fromJust (x `elemIndex` beats) + 1

part1 :: (Char, Char) -> Int
part1 (a, b)
  | a == b = 3 + score b
  | [a, b] `isInfixOf` beats = 6 + score b
  | otherwise = score b

beatX :: Char -> Char
beatX x = cycle ['R', 'P', 'S'] !! (score x + 3)

loseX :: Char -> Char
loseX x = cycle ['R', 'P', 'S'] !! (score x + 1)

part2 :: (Char, Char) -> Int
part2 (a, 'R') = score $ loseX a
part2 (a, 'P') = 3 + score a
part2 (a, 'S') = 6 + score (beatX a)

solve :: IO ()
solve = do
  pairs <- fmap toPair . lines <$> readFile "input/2.txt"
  print $ sum $ part1 <$> pairs
  print $ sum $ part2 <$> pairs
