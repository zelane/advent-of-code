import Data.Char (digitToInt)
import Data.Foldable (foldr')
import qualified Data.Heap as H
import Data.List (foldl1', iterate', sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Point = (Int, Int)

type Grid = [[Int]]

neighbors :: Point -> Point -> S.Set Point
neighbors (mx, my) (x, y) = S.fromList $ filter checkBounds [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    checkBounds (x, y) = x >= 0 && x <= mx && y >= 0 && y <= my

grow :: Grid -> Grid
grow = growH . growW
  where
    growH = concat . take 5 . iterate' incGrid
    growW = foldl1' (zipWith (++)) . take 5 . iterate' incGrid
    incGrid = map (map inc)
    inc x = if x + 1 > 9 then (x + 1) `mod` 9 else x + 1

riskilest :: Grid -> Int
riskilest grid = go startH S.empty []
  where
    startH = H.fromList [(0, (0, 0))]
    end = (length (head grid) - 1, length grid - 1)
    costMap = M.fromList [((x, y), grid !! y !! x) | x <- [0 .. fst end], y <- [0 .. snd end]]
    cost k = costMap M.! k

    go heap visited risks
      | not (null risks) && risk > head risks = head risks
      | current `S.member` visited = go (H.deleteMin heap) visited risks
      | current == end = go (H.deleteMin heap) visited nPaths
      | otherwise = go nHeap nVisited risks
      where
        (risk, current) = H.minimum heap
        nPaths = sort $ risk : risks
        nVisited = S.insert current visited
        toVisit = S.toList $ S.difference (neighbors end current) visited
        new = [(risk + cost p, p) | p <- toVisit]
        nHeap = foldr' H.insert (H.deleteMin heap) new

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let grid = map (map digitToInt) lines
  print $ riskilest grid
  print $ riskilest (grow grid)
