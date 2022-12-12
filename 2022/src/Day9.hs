module Day9 where

import Data.List (nub, scanl')
import Lib (zipPair)

type Point = (Int, Int)

moveHX :: [Point] -> (Char, Int) -> [Point]
moveHX h (dir, dis) = take dis $ drop 1 $ iterate (zipPair (+) t) $ last h
  where
    t = case dir of
      'U' -> (0, 1)
      'D' -> (0, -1)
      'L' -> (-1, 0)
      'R' -> (1, 0)

moveX :: [Point] -> [Point] -> [[Point]]
moveX [] _ = []
moveX (h : hs) knots = newTs : moveX hs newTs
  where
    newTs = [moveT knot prev | (knot, prev) <- zip knots (h : newTs)]

moveT :: Point -> Point -> (Int, Int)
moveT t h = zipPair (+) t tr
  where
    (x, y) = zipPair (-) t h
    tr
      | abs x /= 2 && abs y /= 2 = (0, 0)
      | otherwise = (signum x * (-1), signum y * (-1))

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let ins = (\([a, _], b) -> (a, read b)) . splitAt 2 <$> lines :: [(Char, Int)]
  let heads = concat $ scanl' moveHX [(0, 0)] ins

  print $ length $ nub $ scanl' moveT (0, 0) heads
  print $ length $ nub $ map last $ moveX heads $ replicate 9 (0, 0)
