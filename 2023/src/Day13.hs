module Day13 where

import Data.List (transpose)
import Data.List.Split (splitOn)

check :: Int -> [String] -> Int
check maxDiff s
  | null pairs || null reflections = 0
  | otherwise = sum reflections + 1
  where
    pairs = [idx | (idx, (a, b)) <- zip [0 ..] (zip s (tail s)), diffPair a b <= maxDiff]
    reflections = filter (checkReflection maxDiff s) pairs

checkReflection :: Int -> [String] -> Int -> Bool
checkReflection maxDiff s idx = diff == maxDiff
  where
    diff = sum [diffPair as bs | (as, bs) <- reflection]
    reflection = [(s !! ai, s !! bi) | (ai, bi) <- zip [idx, idx - 1 .. 0] [idx + 1 .. length s - 1]]

diffPair :: String -> String -> Int
diffPair as bs = sum [1 | (a, b) <- zip as bs, a /= b]

checkBoth :: Int -> [String] -> Int
checkBoth maxDiff s = check maxDiff s + (100 * check maxDiff (transpose s))

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let parts = transpose <$> splitOn [""] input

  print $ sum $ checkBoth 0 <$> parts
  print $ sum $ checkBoth 1 <$> parts
