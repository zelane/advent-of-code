module Day4 where

import Data.Function.Memoize (memoize3)
import Data.List (intersect)
import Data.List.Split (splitOn)

type Card = Int

parse :: String -> Card
parse str = length (words win `intersect` words have)
  where
    [win, have] = splitOn "|" $ dropWhile (/= ':') str

scores :: [Int]
scores = 0 : [2 ^ n | n <- [0 ..]]

part2 :: [Card] -> Bool -> Int -> Int
part2 cards original index
  | index >= length cards = 0
  | original = 1 + subScore + part2' cards True (index + 1)
  | otherwise = 1 + subScore
  where
    score = cards !! index
    subScore = sum $ part2' cards False <$> [index + n | n <- [1 .. score]]

part2' :: [Card] -> Bool -> Int -> Int
part2' = memoize3 part2

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let cards = parse <$> lines
  print $ sum $ (scores !!) <$> cards
  print $ part2 cards True 0
