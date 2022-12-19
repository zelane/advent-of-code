module Day15 where

import Data.Bifunctor (bimap)
import Data.Foldable (find)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Lib (parseInts, taxi)

type Point = (Int, Int)

type Range = (Int, Int)

parseL :: String -> (Point, Point)
parseL line = ((x1, y1), (x2, y2))
  where
    [x1, y1, x2, y2] = parseInts line

xrange :: Int -> (Point, Point) -> Range
xrange y (s, b) = if y <= sy + d && y >= sy - d then (sx - width, sx + width) else (0, 0)
  where
    d = taxi s b
    (sx, sy) = s
    width = d - abs (sy - y) -- Offset from Y determines how far left and right we extend

join :: [Range] -> [Range]
join [] = []
join [x] = [x]
join ((a, b) : (c, d) : xs)
  | b >= c = join ((a, max b d) : xs)
  | otherwise = (a, b) : join ((c, d) : xs)

limit :: [Range] -> [Range]
limit ranges = [(max 0 l, min 4000000 r) | (l, r) <- ranges]

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let pairs = parseL <$> input

  let cave = [join $ sortOn fst $ xrange y <$> pairs | y <- [0 .. 4000000]]
  print $ sum $ abs . uncurry (-) <$> cave !! 2000000

  let segment = limit <$> cave
  let (y, [(_, mi), _]) = fromJust $ find ((> 1) . length . snd) $ zip [0 ..] segment
  print $ ((mi + 1) * 4000000) + y
