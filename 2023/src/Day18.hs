module Day18 where

import Numeric (readHex)

type Ins = (String, Int)

dig :: (Int, Int) -> Ins -> (Int, Int)
dig (x, y) ("U", dist) = (x, y - dist)
dig (x, y) ("D", dist) = (x, y + dist)
dig (x, y) ("L", dist) = (x - dist, y)
dig (x, y) ("R", dist) = (x + dist, y)

dehex :: String -> Ins
dehex h = (dirs $ last x, fst . head $ readHex $ init x)
  where
    x = init $ drop 2 h
    dirs '3' = "U"
    dirs '1' = "D"
    dirs '2' = "L"
    dirs '0' = "R"

area :: [Ins] -> Int
area plan = area' `div` 2 + 1
  where
    vertices = scanl dig (0, 0) plan
    area' = sum . zipWith (\(x, y) (x', y') -> (x * y' - x' * y) + abs (x - x' + y - y')) vertices $ tail vertices

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let (planA, planB) = unzip [((a, read b), dehex c) | line <- input, let [a, b, c] = words line]
  print $ area planA
  print $ area planB
