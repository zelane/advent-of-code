module Day10 where

import Data.Maybe (fromJust)

type Pipes = [(Point, Char)]

type Point = (Int, Int)

parse :: [String] -> Pipes
parse ls = concat [[((x, y), c) | (x, c) <- zip [0 ..] line] | (y, line) <- zip [0 ..] ls]

findPaths :: Pipes -> [Point] -> [Point]
findPaths pipes path
  | null nextPoints = path
  | otherwise = findPaths pipes newPaths
  where
    nextPoints = move pipes path
    newPaths = head nextPoints : path

move :: Pipes -> [Point] -> [Point]
move pipes ((x, y) : path) = [p | (mx, my) <- moves c, let p = (x + mx, y + my), p `notElem` path]
  where
    c = fromJust $ lookup (x, y) pipes

moves :: Char -> [Point]
moves a = case a of
  'S' -> [(1, 0), (0, 1)] -- Assuming that S is a F
  'F' -> [(1, 0), (0, 1)]
  '7' -> [(-1, 0), (0, 1)]
  'J' -> [(0, -1), (-1, 0)]
  'L' -> [(1, 0), (0, -1)]
  '-' -> [(1, 0), (-1, 0)]
  '|' -> [(0, -1), (0, 1)]

shoelace :: [(Int, Int)] -> Int
shoelace vertices = abs $ sum products `div` 2
  where
    products = zipWith (\(x1, y1) (x2, y2) -> x1 * y2 - x2 * y1) vertices $ drop 1 $ cycle vertices

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let pipes = parse input
  let start = fst $ head $ filter ((== 'S') . snd) pipes
  let path = findPaths pipes [start]
  print $ length path `div` 2
  let area = shoelace path
  print $ area - abs (length path `div` 2) + 1
