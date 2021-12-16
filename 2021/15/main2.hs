import Algorithm.Search (dijkstra)
import Data.Char (digitToInt)
import Data.List (foldl1', iterate')
import Data.Maybe (fromJust)

type Point = (Int, Int)

type Path = [Point]

type Grid = [[Int]]

neighbors :: Int -> Int -> Point -> [Point]
neighbors mx my (x, y) = filter checkBounds [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    checkBounds (x, y) = x >= 0 && x <= mx && y >= 0 && y <= my

grow :: Grid -> Grid
grow = growH . growW
  where
    growH = concat . take 5 . iterate' incGrid
    growW = foldl1' (zipWith (++)) . take 5 . iterate' incGrid
    incGrid = map (map inc)
    inc x = if x + 1 > 9 then (x + 1) `mod` 9 else x + 1

findPath :: Grid -> Int
findPath grid = fst . fromJust $ dijkstra (neighbors maxX maxY) cost end (0, 0)
  where
    (maxY, maxX) = (length grid - 1, length (head grid) - 1)
    end p = p == (maxX, maxY)
    cost _ (x, y) = grid !! y !! x

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let grid = map (map digitToInt) lines
  print $ findPath grid
  print $ findPath $ grow grid