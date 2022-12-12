module Day6 (solve) where

import Data.List (nub)

findMarker :: Int -> String -> Int
findMarker l s = loop l 0 s
  where
    loop l i s =
      if length (nub $ take l s) == l
        then i + l
        else loop l (i + 1) (drop 1 s)

solve :: IO String -> IO ()
solve file = do
  input <- file
  print $ findMarker 4 input
  print $ findMarker 14 input
