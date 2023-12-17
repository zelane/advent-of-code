module Day12 where

import Data.HashTable.IO qualified as H
import Data.List (intercalate)
import Data.List.Split (splitOn)

type Memo = H.BasicHashTable (Char, String, [Int]) Int

parse :: Memo -> Char -> String -> [Int] -> IO Int
parse memo p s nums
  | null s && not (null nums) = return 0 -- too many nums
  | null nums && elem '#' s = return 0 -- need more nums
  | null nums = return 1
  --
  | a == '.' = parse' memo '.' xs nums
  | a == '?' = (+) <$> parse' memo p ('#' : xs) nums <*> parse' memo p ('.' : xs) nums
  --
  | a == '#' && p == '#' = return 0
  | a == '#' && lenNextD < n = return 0 -- length until next . too short
  --
  | a == '#' = parse' memo '#' (drop (n - 1) xs) ns
  where
    (a : xs) = s
    (n : ns) = nums
    lenNextD = length $ takeWhile (/= '.') s

parse' :: Memo -> Char -> String -> [Int] -> IO Int
parse' m c s xs = do
  existing <- H.lookup m (c, s, xs)
  maybe
    ( do
        next <- parse m c s xs
        H.insert m (c, s, xs) next
        return next
    )
    return
    existing

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let springs = [(a, read <$> splitOn "," b) | line <- input, let [a, b] = words line]
  let unfolded = [(intercalate "?" (replicate 5 s), concat $ replicate 5 c) | (s, c) <- springs]

  memo <- H.new
  part1 <- mapM (uncurry (parse' memo '.')) springs
  print $ sum part1

  part2 <- mapM (uncurry (parse' memo '.')) unfolded
  print $ sum part2
