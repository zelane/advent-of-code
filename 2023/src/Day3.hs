module Day3 where

import Data.Char (isDigit)
import Data.List (intersect)
import Data.Map qualified as M

type Point = (Int, Int)

type Schematic = M.Map Point Char

type Numbers = (Int, [Point])

checkAdj :: Schematic -> Point -> Bool
checkAdj s p = any (\c -> not (isDigit c || c == '.')) adj
  where
    adj = (\k -> M.findWithDefault '.' k s) <$> adjacent p

adjacent :: Point -> [Point]
adjacent (x, y) = [(x + xm, y + ym) | ym <- [-1 .. 1], xm <- [-1 .. 1]]

findNums :: [(Point, Char)] -> [Numbers]
findNums [] = []
findNums (x : xs)
  | isDigit $ snd x = (int, fst <$> intPoints) : findNums next
  | otherwise = findNums xs
  where
    intPoints = x : takeWhile (isDigit . snd) xs
    int = read $ snd <$> intPoints :: Int
    next = drop (length intPoints) xs

checkNum :: Schematic -> [Numbers] -> [Int]
checkNum s nums = fst <$> filter checkPoints nums
  where
    checkPoints (_, points) = any (checkAdj s) points

checkGear :: [Numbers] -> Point -> Int
checkGear nums p
  | length adjNums == 2 = product $ fst <$> adjNums
  | otherwise = 0
  where
    adjNums = filter (not . null . (`intersect` adjPoints) . snd) nums
    adjPoints = adjacent p

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let list = [((x, y), c) | (line, y) <- zip lines [0 ..], (c, x) <- zip line [0 ..]]
  let sche = M.fromList list
  let nums = findNums list
  print $ sum $ checkNum sche nums

  let stars = [x | (x, '*') <- list]
  let ratio = sum $ checkGear nums <$> stars
  print ratio
