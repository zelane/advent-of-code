import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Split (splitOn)

calcFuel :: (Num a) => (a -> a -> a) -> [a] -> a -> a
calcFuel f x p = sum $ map (f p) x

main :: IO ()
main = do
  vs <- readFile "input.txt"
  let vals = map read $ splitOn "," vs :: [Int]
  let f p x = abs (x - p)
  let f2 p x = sum [0 .. abs (x - p)]

  print $ minimum $ map (calcFuel f vals) [0 .. 500]
  print $ minimum $ parMap rpar (calcFuel f2 vals) [0 .. 500]
