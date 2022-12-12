module Day10 where

import Data.List.Split (chunksOf)

parse :: String -> (String, Int)
parse s = if take 4 s == "noop" then ("noop", 0) else (init ins, vi)
  where
    vi = if head v == '-' then -(read $ tail v) else read v
    (ins, v) = splitAt 5 s

run :: [Int] -> (String, Int) -> [Int]
run sigs (op, v)
  | op == "noop" = [x]
  | op == "addx" = [x, x + v]
  where
    x = last sigs

draw :: (Int, Int) -> Char
draw (i, v) = if im >= (v - 1) && im <= (v + 1) then '#' else '.'
  where
    im = i `mod` 40

strength :: [Int] -> Int -> Int
strength sigs i = (sigs !! (i - 1)) * i

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let ins = parse <$> lines
  let sigs = concat $ scanl run [1] ins
  print $ sum $ strength sigs <$> ([20, 60, 100, 140, 180, 220] :: [Int])

  let draws = draw <$> zip [0 ..] sigs
  mapM_ print $ chunksOf 40 draws
