import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

findClosest :: Int -> Int -> Int
findClosest target time = head loop - target
  where
    loop = filter (> target) $ iterate (+ time) time

find :: [(Int, Int)] -> Int -> Int -> Int
find [] _ t = t
find ((offset, prime) : xs) inc t = find xs newInc lcm
  where
    increments = [t, t + inc ..]
    lcm = head $ filter (\inc -> (inc + offset) `mod` prime == 0) increments
    newInc = inc * prime

parse :: (Int, String) -> (Int, Int)
parse (a, b) = (a, read b) :: (Int, Int)

main :: IO ()
main = do
  lines <- fmap lines (readFile "input.txt")
  let [time, _buses] = lines
  let arrival = read time :: Int
  let buses = map read $ filter (/= "x") $ splitOn "," _buses :: [Int]
  let (a1, b1) = minimumBy (comparing snd) $ zip buses $ map (findClosest arrival) buses
  print $ a1 * b1

  let buses2 = map parse $ filter ((/= "x") . snd) $ zip [0 ..] $ splitOn "," _buses
  print $ find buses2 1 1
