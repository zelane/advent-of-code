module Day3 (solve) where

import Text.Regex.TDFA (getAllTextMatches, (=~))

mul :: String -> Int
mul s = read a * read b
  where
    [a, b] = getAllTextMatches $ s =~ ("[0-9]+" :: String) :: [String]

run :: Bool -> [String] -> Int
run _ [] = 0
run on (s : sx) = case take 3 s of
  "mul" -> if on then mul s + run on sx else run on sx
  "don" -> run False sx
  "do(" -> run True sx

solve :: IO String -> IO ()
solve file = do
  input <- file

  let part1 = getAllTextMatches $ input =~ ("mul\\([0-9]+,[0-9]+\\)" :: String) :: [String]
  let part2 = getAllTextMatches $ input =~ ("mul\\([0-9]+,[0-9]+\\)|don\'t\\(\\)|do\\(\\)" :: String) :: [String]
  print $ run True part1
  print $ run True part2
