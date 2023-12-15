module Day14 where

import Data.List (intercalate, iterate', sortBy, transpose)
import Data.List.Split (splitOn)

tilt :: [String] -> [String]
tilt s = transpose (sort' <$> transpose s)

sort' :: String -> String
sort' s = intercalate "#" $ sortBy sort'' <$> parts
  where
    parts = splitOn "#" s

sort'' :: Char -> Char -> Ordering
sort'' '#' _ = LT
sort'' 'O' '#' = GT
sort'' '.' _ = GT
sort'' _ _ = EQ

calc :: [(Int, String)] -> Int
calc [] = 0
calc ((i, x) : xs) = i * length (filter (== 'O') x) + calc xs

cycleLength :: (Eq a) => [a] -> (Int, Int)
cycleLength [] = (0, 0)
cycleLength (x : xs) =
  let loop _ _ _ [] = (0, 0)
      loop pow lam x (y : ys)
        | x == y = (pow, lam)
        | pow == lam = loop (2 * pow) 1 y ys
        | otherwise = loop pow (1 + lam) x ys
   in loop 1 1 x xs

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let sorted = tilt input
  print $ calc $ zip [length sorted, length sorted - 1 ..] sorted

  let iter = iterate' (transpose . reverse . tilt) input
  let loads = (\s -> calc $ zip [length s, length s - 1 ..] s) <$> iter
  let (prefix, cycl) = cycleLength loads
  let cyc = take cycl $ drop prefix loads
  let idx = (4000000000 - prefix) `mod` cycl
  print $ cyc !! idx
