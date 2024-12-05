{-# LANGUAGE NoOverloadedLists #-}

module Day4 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (iterate')
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)

type Grid = M.Map (Int, Int) Char

type Pos = (Int, Int)

checkLine :: Grid -> Pos -> (Int, Int) -> Int
checkLine g pos (mx, my) = if catMaybes points == "XMAS" then 1 else 0
  where
    points = (`M.lookup` g) <$> take 4 (iterate' (bimap (+ mx) (+ my)) pos)

checkAll :: Grid -> Pos -> Int
checkAll g pos = sum $ checkLine g pos <$> [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1]]

checkCorners :: Grid -> Pos -> Int
checkCorners g (x, y) = if corners `elem` ["SMSM", "MSMS", "SSMM", "MMSS"] then 1 else 0
  where
    corners = mapMaybe (`M.lookup` g) [(x + mx, y + my) | mx <- [-1, 1], my <- [-1, 1]]

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let grid = M.fromList [((x, y), lines !! y !! x) | x <- [0 .. length (head lines) - 1], y <- [0 .. length lines - 1]]
  let xs = M.keys $ M.filter (== 'X') grid
  print $ sum $ checkAll grid <$> xs

  let as = M.keys $ M.filter (== 'A') grid
  print $ sum $ checkCorners grid <$> as
