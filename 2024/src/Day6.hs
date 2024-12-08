module Day6 where

import Data.Foldable (Foldable (foldr'))
import Data.HashSet qualified as S
import Data.Maybe (fromJust, isNothing)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (0, -1) = (1, 0)
turnRight (1, 0) = (0, 1)
turnRight (0, 1) = (-1, 0)
turnRight (-1, 0) = (0, -1)

type Guard = ((Int, Int), (Int, Int)) -- Position, Direction

run2 :: S.HashSet (Int, Int) -> Int -> S.HashSet Guard -> Guard -> Maybe (S.HashSet (Int, Int))
run2 walls size path ((x, y), (mx, my))
  | x == -1 || y == -1 || x > size || y > size = Just S.empty
  | (nDir, nPos) `S.member` path = Nothing
  | nPos `S.member` walls = run2 walls size nPath ((x, y), nDir)
  | otherwise = S.insert (x, y) <$> run2 walls size nPath (nPos, (mx, my))
  where
    nPos = (x + mx, y + my)
    nDir = turnRight (mx, my)
    nPath = S.insert (nDir, nPos) path

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let size = length lines - 1
  let grid = [((x, y), lines !! y !! x) | x <- [0 .. size], y <- [0 .. size]]
  let start = head [xy | (xy, n) <- grid, n == '^']
  let walls = S.fromList [xy | (xy, n) <- grid, n == '#']

  let path = fromJust $ run2 walls size S.empty (start, (0, -1))
  print $ length path
  print $ length $ filter (\block -> isNothing $ run2 (S.insert block walls) size S.empty (start, (0, -1))) $ S.toList path
