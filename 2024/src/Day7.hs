module Day7 where

import Data.List.Split (splitOn)

test :: [Int -> Int -> Int] -> Int -> [Int] -> Int
test funcs target (a : xs) = if go a xs then target else 0
  where
    go acc [] = acc == target
    go acc (x : xs) = (acc <= target) && any (\f -> go (f acc x) xs) funcs

con :: Int -> Int -> Int
con x y = x * nearestBase10 y + y
  where
    nearestBase10 n
      | n < 10 = 10
      | otherwise = 10 * nearestBase10 (n `div` 10)

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let eq = [(read t, read <$> words xs) | [t, xs] <- splitOn ":" <$> lines]
  print $ sum [test [(+), (*)] t nums | (t, nums) <- eq]
  print $ sum [test [(+), (*), con] t nums | (t, nums) <- eq]
