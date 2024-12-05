module Day5 where

import Data.List (elemIndex, intersect, sortBy, (\\))
import Data.List.Split (splitOn)
import Data.Maybe (isJust)

psort :: [String] -> String -> String -> Ordering
psort rules a b
  | isJust $ elemIndex (b ++ "|" ++ a) rules = GT
  | otherwise = EQ

mid :: [String] -> Int
mid s = read $ s !! (length s `div` 2)

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let (rules : ps : _) = splitOn [""] lines
  let pages = splitOn "," <$> ps
  let sorted = sortBy (psort rules) <$> pages

  print $ sum $ mid <$> sorted `intersect` pages
  print $ sum $ mid <$> sorted \\ pages
