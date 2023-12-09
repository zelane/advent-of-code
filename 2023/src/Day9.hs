module Day9 where

diff :: [Int] -> [Int]
diff xs = zipWith (-) (tail xs) xs

diffs :: [Int] -> [[Int]]
diffs = takeWhile (any (/= 0)) . iterate diff

next :: [Int] -> Int
next = foldr ((+) . last) 0 . diffs

prev :: [Int] -> Int
prev = foldr ((-) . head) 0 . diffs

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let history = map read . words <$> input :: [[Int]]
  print $ sum $ next <$> history
  print $ sum $ prev <$> history
