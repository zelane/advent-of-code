module Day10 where

import Data.Char (digitToInt)
import Data.List (nub)

trails :: [((Int, Int), Int)] -> (Int, Int) -> [(Int, Int)]
trails map' pos@(x, y) = case lookup pos map' of
  Nothing -> []
  Just cur
    | cur == 9 -> [pos]
    | otherwise -> concat [trails map' p | p <- neigh, lookup p map' == Just (cur + 1)]
    where
      neigh = [(x + mx, y + my) | (mx, my) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let map' = [((x, y), digitToInt c) | (y, row) <- zip [0 ..] lines, (x, c) <- zip [0 ..] row]
  let zeroes = fst <$> filter ((== 0) . snd) map'
  print $ sum $ length . nub . trails map' <$> zeroes
  print $ sum $ length . trails map' <$> zeroes
