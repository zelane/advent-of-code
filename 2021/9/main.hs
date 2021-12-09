import Data.Char (digitToInt)
import Data.List (nub, sortOn)
import Data.Map.Strict (Map, filterWithKey, fromList, keys, lookup, union, (!))
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Ord (Down (Down))
import Prelude hiding (lookup)

type Point = (Int, Int)

type Grid = Map Point Int

neighbors :: Point -> [Point]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isLow :: Point -> Grid -> Bool
isLow (x, y) grid = grid ! (x, y) < minimum neighs
  where
    neighs = mapMaybe (`lookup` grid) (neighbors (x, y))

traceLine :: (Int, Int) -> Point -> Int -> Grid -> [Point]
traceLine (modX, modY) (x, y) v grid
  | isNothing nv = []
  | fromJust nv == 9 = []
  | fromJust nv > v = (nx, ny) : traceLine (modX, modY) (nx, ny) (fromJust nv) grid
  | otherwise = []
  where
    (nx, ny) = (x + modX, y + modY)
    nv = lookup (nx, ny) grid

basin :: Grid -> Point -> [Point]
basin grid (x, y) =
  if null lines
    then []
    else lines ++ concatMap (basin grid) lines
  where
    points = mapMaybe (`lookup` grid) lines
    v = grid ! (x, y)
    lines = nub $ concatMap (\(mx, my) -> traceLine (mx, my) (x, y) v grid) trans
    trans = [(0, 1), (0, -1), (1, 0), (-1, 0)]

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let ints = map digitToInt <$> lines
  let grid = fromList $ concat [[((x, y), v) | (x, v) <- zip [0 ..] row] | (y, row) <- zip [0 ..] ints]

  let lows = filterWithKey (\k v -> isLow k grid) grid
  print $ foldr (\a x -> a + x + 1) 0 lows

  let basins = nub . basin grid <$> keys lows
  print $ product $ (+ 1) . length <$> take 3 (sortOn (Down . length) basins)
