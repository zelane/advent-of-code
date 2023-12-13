module Day11 where

import Data.List (tails, transpose)

expand :: Int -> [String] -> [String]
expand _ [] = []
expand f ls = foldl (\a x -> a <> xx x) [] ls
  where
    xx :: String -> [String]
    xx l = if all (== '.') l then replicate f l else [l]

expand' :: Int -> [String] -> [String]
expand' f = expand f . transpose . expand f

taxi :: (Num a) => (a, a) -> (a, a) -> a
taxi (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

sumDists :: [String] -> Int
sumDists input = sum dists
  where
    galaxies = concat [[(x, y) | (x, c) <- zip [0 ..] line, c == '#'] | (y, line) <- zip [0 ..] input]
    args = zip galaxies $ tails galaxies
    dists = (\(a, xs) -> sum $ taxi a <$> xs) <$> args

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file

  let dist1 = sumDists input
  let dist2 = sumDists $ expand' 2 input
  let diff = dist2 - dist1
  print dist2
  print $ dist1 + ((1000000 - 1) * diff)
