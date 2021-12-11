import Data.Char (digitToInt)
import Data.List (findIndex)
import Data.Map (Map, adjust, difference, elems, empty, filter, fromList, keys, map, union)
import Prelude hiding (filter, map)

type Point = (Int, Int)

type Grid = Map Point Int

spark :: Grid -> Point -> Grid
spark grid (x, y) = foldl (flip (adjust (+ 1))) grid neighbors
  where
    neighbors = [(x + xm, y + ym) | xm <- [-1 .. 1], ym <- [-1 .. 1]]

parse :: (Int, Grid) -> Grid -> (Int, Grid)
parse (s, grid) sparked =
  if null toSpark
    then (s, nulled)
    else parse (s + length toSpark, newGrid) (sparked `union` toSpark)
  where
    sparks = filter (> 9) grid
    toSpark = difference sparks sparked
    newGrid = foldl spark grid (keys toSpark)
    nulled = map (\x -> if x > 9 then 0 else x) newGrid

step :: (Int, Grid) -> (Int, Grid)
step (s, grid) = parse (s, map (+ 1) grid) empty

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let ints = fmap digitToInt <$> lines
  let grid = fromList $ concat [[((x, y), v) | (x, v) <- zip [0 ..] row] | (y, row) <- zip [0 ..] ints]

  let steps = iterate step (0, grid)
  print $ fst $ steps !! 100
  print $ findIndex (all (== 0) . elems . snd) steps
