module Day6 where

parse :: String -> [(Int, Int)]
parse s = [(read a, read b) | (a, b) <- zip t d]
  where
    [t, d] = tail . words <$> lines s

parse2 :: String -> (Int, Int)
parse2 s = (read t, read d)
  where
    [t, d] = concat . tail . words <$> lines s

scores :: Int -> [Int]
scores d = calc d <$> [0 .. d]
  where
    calc dist hold = hold * (dist - hold)

wins :: Int -> Int -> Int
wins d r = length $ filter (> r) $ scores d

solve :: IO String -> IO ()
solve file = do
  input <- file
  let parsed = parse input
  print $ product $ uncurry wins <$> parsed

  let parsed2 = parse2 input
  print $ uncurry wins parsed2
