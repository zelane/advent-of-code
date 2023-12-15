module Day12 where

import Data.Foldable (foldl')
import Data.List (group, groupBy, intercalate)
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Text qualified as T
import Debug.Trace (traceShow)

parse :: Char -> T.Text -> [Int] -> Int
parse p s nums
  | T.null s && not (null nums) = 0 -- too many nums
  | null nums && T.elem '#' s = 0 -- need more nums
  | null nums && T.all (== '.') s = 1 -- string ends in dots
  --
  | a == '.' = parse '.' xs nums
  | a == '?' && p == '#' = parse p (T.cons '.' xs) nums
  | a == '?' = parse '.' (T.cons '#' xs) nums + parse p (T.cons '.' xs) nums
  --
  | a == '#' && p == '#' = 0
  | a == '#' && n > T.length s = 0 -- num is longer than rest of input
  | a == '#' && lenH > n = 0 -- hashes are longer than next num
  | a == '#' && lenNextD < n = 0 -- length until next . too short
  --
  | a == '#' = parse '#' (T.drop (n - 1) xs) ns -- need to add .
  --
  where
    lenH = T.length $ T.takeWhile (== '#') s
    lenNextD = T.length $ T.takeWhile (/= '.') s
    a = T.head s
    xs = T.tail s
    (n : ns) = nums

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let springs = [(T.pack a, read <$> splitOn "," b :: [Int]) | line <- input, let [a, b] = words line]
  let unfolded = [(T.intercalate "?" (replicate 5 s), concat $ replicate 5 c) | (s, c) <- springs]

  print $ sum $ uncurry (parse '.') <$> springs
  print ""
  -- print $ (\(a, b) -> parse '.' a b) $ unfolded !! 8

  print $ foldl' (\acc (a, b) -> acc + parse '.' a b) 0 unfolded
