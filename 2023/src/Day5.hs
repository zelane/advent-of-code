module Day5 where

import Data.Foldable (foldl')
import Data.Ix (inRange)
import Data.List.Split (chunksOf, splitOn)

type Group = [Range]

type Range = ((Int, Int), Int)

type Seeds = [Int]

type Almanac = [Group]

parse :: String -> (Seeds, Almanac)
parse str = (seeds, parseGroup . lines <$> groups)
  where
    seeds = read <$> tail (words seed)
    (seed : groups) = splitOn "\n\n" str

parseGroup :: [String] -> Group
parseGroup (_ : xs) = parseRange <$> xs

parseRange :: String -> Range
parseRange str = ((src, src + rng - 1), dst - src)
  where
    [dst, src, rng] = read <$> words str

applyGroup :: Group -> (Int, (Int, Int)) -> (Int, (Int, Int))
applyGroup group (seed, (maxSeed, diff))
  | null range = (seed, (maxSeed, diff))
  | otherwise = (seed + length, newLimits)
  where
    range = filter (\(r, _) -> inRange r seed) group
    [((_, d), length)] = range
    newLimits = (min maxSeed (d - diff), diff + length)

run :: Almanac -> (Int, Int) -> [Int]
run alm (seed, max)
  | seed <= max = location : run alm (skip + 1, max)
  | otherwise = []
  where
    (location, (skip, _)) = foldl' (flip applyGroup) (seed, (max, 0)) alm

solve :: IO String -> IO ()
solve file = do
  input <- file
  let (seeds, almanac) = parse input
  print $ minimum $ minimum . run almanac <$> zip seeds seeds
  print $ minimum $ minimum . run almanac <$> [(a, a + b) | [a, b] <- chunksOf 2 seeds]
