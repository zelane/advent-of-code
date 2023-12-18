module Day17 where

import Algorithm.Search (dijkstraAssoc)
import Data.Char (digitToInt)
import Data.HashMap.Strict qualified as M
import Data.Maybe (fromJust, fromMaybe)

type Grid = M.HashMap (Int, Int) Int

type Point = (Int, Int)

type Step = (Point, Point, (Int, Int), Int) -- Current pos, prev pos, prev dir, same dir count

type Limits = (Int, Int) -- Minimum in same dir, maximum in same dir

getNext :: Limits -> Grid -> Step -> [(Step, Int)]
getNext (dMin, dMax) g (p@(x, y), prevP, prevD, dc) = adj
  where
    adj = [((next, p, dir, newDc dir), cost next) | dir@(mx, my) <- dirs', let next = (x + mx, y + my), cost next > 0, next /= prevP]
    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    dirs'
      | p == (0, 0) = [(0, 1), (1, 0)]
      | dc < dMin = [prevD]
      | dc == dMax = filter (/= prevD) dirs
      | otherwise = dirs
    cost x = fromMaybe 0 (M.lookup x g)
    newDc x = if x == prevD then dc + 1 else 1

isEnd :: Limits -> Point -> Step -> Bool
isEnd (dMin, _) end (p, _, _, dc) = p == end && dc >= dMin

search :: Limits -> Point -> Grid -> Int
search limits end g = fst $ fromJust $ dijkstraAssoc (getNext limits g) (isEnd limits end) ((0, 0), (0, 0), (0, 0), 0)

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let grid = M.fromList $ concat [[((x, y), digitToInt c) | (x, c) <- zip [0 ..] line] | (y, line) <- zip [0 ..] input]
  let end = maximum $ M.keys grid

  print $ search (0, 3) end grid
  print $ search (4, 10) end grid
