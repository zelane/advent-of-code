module Day1 (solve) where

import Data.List (sort)
import Data.List.Split (splitOn)

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let elves = splitOn [""] lines
      sums = reverse $ sort $ sum . fmap read <$> elves
  print $ head sums
  print $ sum $ take 3 sums
