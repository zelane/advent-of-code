module Day10 where

import Data.Maybe (fromJust)

type Pipes = [(Point, Char)]

type Point = (Int, Int)

parse :: [String] -> Pipes
parse lines = concat [[((x, y), c) | (x, c) <- zip [0 ..] line] | (y, line) <- zip [0 ..] lines]

findPaths :: Pipes -> [Point] -> [[Point]]
findPaths pipes path
  | null nextPoints = [path]
  | otherwise = concatMap (findPaths pipes) newPaths
  where
    nextPoints = move pipes path
    newPaths = [p : path | p <- nextPoints]
    next = findPaths pipes <$> newPaths

move :: Pipes -> [Point] -> [Point]
move pipes ((x, y) : ps) = [p | (mx, my) <- moves c, let p = (x + mx, y + my), p `notElem` ps]
  where
    c = fromJust $ lookup (x, y) pipes

moves :: Char -> [Point]
moves a = case a of
  'S' -> [(0, 1), (1, 0)]
  'F' -> [(1, 0), (0, 1)]
  '7' -> [(-1, 0), (0, 1)]
  'J' -> [(0, -1), (-1, 0)]
  'L' -> [(1, 0), (0, -1)]
  '-' -> [(1, 0), (-1, 0)]
  '|' -> [(0, -1), (0, 1)]

findMid :: [Point] -> [Point] -> Int
findMid (a : as) (b : bs) = if a == b then 0 else 1 + findMid as bs

shoelace :: [(Int, Int)] -> Int
shoelace vertices = abs $ sum products `div` 2
  where
    products = zipWith (\(x1, y1) (x2, y2) -> x1 * y2 - x2 * y1) vertices (drop 1 (cycle vertices))

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let pipes = parse input
  let start = fst $ head $ filter ((== 'S') . snd) pipes
  let [a, b] = findPaths pipes [start]
  print $ length a `div` 2
  print $ findMid a b + 1
  let area = shoelace a
  print $ area - abs (length a `div` 2) + 1
