import Data.Char (digitToInt)
import Data.List (sortOn, union)
import Data.Map.Strict (Map, filterWithKey, fromList, keys, lookup, (!))
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Ord (Down (Down))
import Prelude hiding (lookup)

type Point = (Int, Int)

type Grid = Map Point Int

isLow :: Point -> Grid -> Bool
isLow (x, y) grid = grid ! (x, y) < minimum neighs
  where
    neighs = mapMaybe (`lookup` grid) (neighbors (x, y))
    neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

traceLine :: Point -> Grid -> (Int, Int) -> [Point]
traceLine (x, y) grid (modX, modY)
  | isNothing nv = []
  | fromJust nv == 9 = []
  | fromJust nv > grid ! (x, y) = (nx, ny) : traceLine (nx, ny) grid (modX, modY)
  | otherwise = []
  where
    (nx, ny) = (x + modX, y + modY)
    nv = lookup (nx, ny) grid

basin :: Grid -> Point -> [Point]
basin grid (x, y) = lines `union` concatMap (basin grid) lines
  where
    lines = concatMap (traceLine (x, y) grid) trans
    trans = [(0, 1), (0, -1), (1, 0), (-1, 0)]

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let ints = map digitToInt <$> lines
  let grid = fromList $ concat [[((x, y), v) | (x, v) <- zip [0 ..] row] | (y, row) <- zip [0 ..] ints]

  let lows = filterWithKey (\k v -> isLow k grid) grid
  print $ foldr (\a x -> a + x + 1) 0 lows

  let basins = basin grid <$> keys lows
  print $ product $ (+ 1) . length <$> take 3 (sortOn (Down . length) basins)
