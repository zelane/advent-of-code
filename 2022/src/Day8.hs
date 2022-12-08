module Day8 (solve) where

import Data.Char (digitToInt)
import Data.Matrix qualified as M
import Data.Vector qualified as V
import Lib (countUntil)

applyT :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
applyT maxXY (x, y) (tx, ty) = takeWhile inbound $ drop 1 $ iterate trans (x, y)
  where
    trans (a, b) = (a + tx, b + ty)
    inbound (a, b) = a >= 0 && b >= 0 && a < maxXY && b < maxXY

visible :: M.Matrix Int -> (Int, Int) -> Int -> Bool
visible grid (x, y) val = or $ all (< val) <$> views grid (x, y) val

score :: M.Matrix Int -> (Int, Int) -> Int -> Int
score grid (x, y) val = product $ countUntil (< val) <$> views grid (x, y) val

views :: M.Matrix Int -> (Int, Int) -> Int -> [[Int]]
views grid (x, y) val = heights . applyT maxXY (x, y) <$> [(0, -1), (0, 1), (-1, 0), (1, 0)]
  where
    maxXY = M.rows grid
    heights xs = (M.!) grid <$> xs

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let matrix = M.fromLists $ fmap digitToInt <$> lines :: M.Matrix Int
  print $ length $ V.filter (== True) $ M.flatten $ M.imap (visible matrix) matrix
  let scores = M.flatten $ M.imap (score matrix) matrix
  print $ maximum scores
