module Day1 (solve) where

import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)

digits :: [String]
digits = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

match :: [String] -> String -> String
match toMatch s = fromMaybe (match toMatch $ tail s) match_
  where
    match_ = find (`isPrefixOf` s) toMatch

fix :: String -> String
fix "one" = "1"
fix "two" = "2"
fix "three" = "3"
fix "four" = "4"
fix "five" = "5"
fix "six" = "6"
fix "seven" = "7"
fix "eight" = "8"
fix "nine" = "9"
fix x = x

parse :: [String] -> String -> Int
parse toMatch str = read $ fix (match toMatch str) ++ fix (reverse (match revM rev))
  where
    rev = reverse str
    revM = reverse <$> toMatch

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  print $ sum $ parse digits <$> lines
  print $ sum $ parse (digits ++ numbers) <$> lines
