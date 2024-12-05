module Day2 where

import Data.Ix (Ix (inRange))

safe :: [Int] -> Bool
safe x = (inc || dec) && dif
  where
    diffs = zipWith (-) x (tail x)
    inc = all (< 0) diffs
    dec = all (> 0) diffs
    dif = all (inRange (1, 3) . abs) diffs

removeEach :: [a] -> [[a]]
removeEach xs = xs : [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let levels = fmap read . words <$> lines :: [[Int]]

  let variants = removeEach <$> levels
  print $ length $ filter (safe . head) variants
  print $ length $ filter (any safe) variants
