module Day1 (solve) where

import Data.List (sort)

parse :: [String] -> ([Int], [Int])
parse [] = ([], [])
parse (line : xs) = (a : a2, b : b2)
  where
    [a, b] = read <$> words line :: [Int]
    (a2, b2) = parse xs

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let (left, right) = parse lines
  print $ sum $ zipWith (\a b -> abs $ a - b) (sort left) (sort right)
  print $ sum $ [a * length (filter (== a) right) | a <- left]
