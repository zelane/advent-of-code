module Day9 where

diff :: [Int] -> [Int]
diff = zipWith (-) <$> tail <*> id

diffs :: [Int] -> [[Int]]
diffs = takeWhile (any (/= 0)) . iterate diff

next :: [Int] -> Int
next = foldr ((+) . last) 0 . diffs

prev :: [Int] -> Int
prev = foldr ((-) . head) 0 . diffs

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let history = map read . words <$> input
  print $ sum $ next <$> history
  print $ sum $ prev <$> history
