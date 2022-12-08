module Day4 (solve) where

import Data.List (intersect, (\\))
import Text.Regex.TDFA (getAllTextMatches, (=~))

type Assign = ([Int], [Int])

parse :: String -> Assign
parse s = ([a .. b], [c .. d])
  where
    [a, b, c, d] = read <$> getAllTextMatches (s =~ ("([0-9]+)" :: String)) :: [Int]

part1 :: Assign -> Bool
part1 (a, b) = null (a \\ b) || null (b \\ a)

part2 :: Assign -> Bool
part2 (a, b) = not $ null (a `intersect` b)

solve :: IO String -> IO ()
solve file = do
  lines <- fmap parse . lines <$> file
  print $ length $ filter part1 lines
  print $ length $ filter part2 lines
