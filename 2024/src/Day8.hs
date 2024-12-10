module Day8 where

import Data.List (nub)

antinodes :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodes size (x, y) (x2, y2) = takeWhile inbounds $ tail $ iterate mod' (x, y)
  where
    mod' (a, b) = (a + (x2 - x), b + (y2 - y))
    inbounds (x, y) = x >= 0 && x <= size && y >= 0 && y <= size

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let size = length lines - 1
  let antennas = [((x, y), c) | (y, row) <- zip [0 ..] lines, (x, c) <- zip [0 ..] row, c /= '.']
  let pairs = [(p, p2) | (p2, c2) <- antennas, (p, c) <- antennas, c == c2, p /= p2]

  print $ length $ nub $ concatMap (take 1 . drop 1 . uncurry (antinodes size)) pairs
  print $ length $ nub $ concatMap (uncurry (antinodes size)) pairs
