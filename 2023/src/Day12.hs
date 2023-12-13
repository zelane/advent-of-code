module Day12 where

import Data.Function.Memoize (memoize3)
import Data.List (group, groupBy, intercalate)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

simp :: (String, [Int]) -> (String, [Int])
simp (s, nums) = (head <$> groupBy (\a b -> a == '.' && b == '.') s, nums)

parse2 :: Char -> String -> [Int] -> [String]
parse2 p s nums
  | null s && not (null nums) = [] -- too many nums
  | null nums && elem '#' s = [] -- need more nums
  | null nums && all (== '.') s = [s] -- string ends in dots
  --
  | a == '.' = ("." <>) <$> parse2' '.' xs nums
  | a == '?' && p == '#' = parse2' p ('.' : xs) nums
  | a == '?' = parse2' '.' ('#' : xs) nums <> parse2' p ('.' : xs) nums
  --
  | a == '#' && p == '#' = []
  | a == '#' && n > length s = [] -- num is longer than rest of input
  | a == '#' && lenH > n = [] -- hashes are longer than next num
  | a == '#' && lenNextD < n = [] -- length until next . too short
  --
  | a == '#' = (replicate n '#' <>) <$> parse2' '#' (drop (n - 1) xs) ns -- need to add .
  --
  where
    lenH = length $ takeWhile (== '#') s
    lenNextD = length $ takeWhile (/= '.') s
    (a : xs) = s
    (n : ns) = nums

parse2' :: Char -> String -> [Int] -> [String]
parse2' = memoize3 parse2

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let springs = [(a, read <$> splitOn "," b :: [Int]) | line <- input, let [a, b] = words line]
  let t = 0
  let unfolded = [(intercalate "?" (replicate 5 s), concat $ replicate 5 c) | (s, c) <- springs]
  print $ snd $ unfolded !! t
  print $ fst $ unfolded !! t
  mapM_ print $ uncurry (parse2 '.') $ springs !! t

  -- print $ springs !! 1
  -- mapM_ print $ uncurry (parse2 '.') $ springs !! 1
  -- print $ springs !! 2
  -- mapM_ print $ uncurry (parse2 '.') $ springs !! 2
  -- print $ springs !! 3
  -- mapM_ print $ uncurry (parse2 '.') $ springs !! 3
  -- print $ springs !! 4
  -- mapM_ print $ uncurry (parse2 '.') $ springs !! 4
  -- print $ springs !! 5
  -- mapM_ print $ uncurry (parse2 '.') $ springs !! 5

  print $ sum $ length . uncurry (parse2' '.') . simp <$> springs
  print ""

  print $ sum $ length . uncurry (parse2' '.') . simp <$> unfolded

-- 9016 too high
-- 525152 too low
