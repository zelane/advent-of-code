module Day14 where

import Data.Foldable (foldl')
import Data.List (intercalate, iterate', sortBy, transpose)
import Data.List.Split (splitOn)
import Data.Text qualified as T

tilt :: [String] -> [String]
tilt s = transpose (sort' <$> transpose s)

sort' :: String -> String
sort' s = intercalate "#" $ sortBy sort'' <$> splitOn "#" s
  where
    sort'' '.' _ = GT
    sort'' _ _ = EQ

calcLoad :: [String] -> Int
calcLoad xs = sum [i * length (filter (== 'O') x) | (i, x) <- zip [1 ..] (reverse xs)]

cycleLength :: (Eq a) => [a] -> (Int, Int)
cycleLength (x : xs) = loop 1 1 x xs
  where
    loop pow lam x (y : ys)
      | x == y = (pow, lam)
      | pow == lam = loop (2 * pow) 1 y ys
      | otherwise = loop pow (1 + lam) x ys

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  print $ calcLoad $ tilt input
  let loads = calcLoad <$> iterate' (transpose . reverse . tilt) input
  let (prefixL, cycleL) = cycleLength loads
  let idx = prefixL + (4000000000 - prefixL) `mod` cycleL
  print $ loads !! idx
