import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Regex.Posix ((=~))

rotate :: Char -> Int -> Char
rotate curDir deg = newDir
  where
    newDir = dirs !! (fromJust (elemIndex curDir dirs) + shift)
    dirs = cycle ['N', 'E', 'S', 'W']
    rotations = deg `div` 90
    shift = if rotations > 0 then rotations else rotations + 4

move :: (Char, Int, Int) -> Int -> (Int, Int)
move (dir, x, y) val = case dir of
  'N' -> (x, y + val)
  'S' -> (x, y - val)
  'E' -> (x + val, y)
  'W' -> (x - val, y)

moveShip :: [String] -> (Char, Int, Int) -> (Char, Int, Int)
moveShip [] result = result
moveShip (i : ins) (dir, x, y) = moveShip ins result
  where
    (d, val) = parseCmd i
    result
      | d == 'L' = (rotate dir (- val), x, y)
      | d == 'R' = (rotate dir val, x, y)
      | d == 'F' = set dir (move (dir, x, y) val)
      | otherwise = set dir (move (d, x, y) val)
    set d (a, b) = (d, a, b)

rotateWaypointL :: Num a => (b, a) -> (a, b)
rotateWaypointL (x, y) = (- y, x)

rotateWaypointR :: Num b => (b, a) -> (a, b)
rotateWaypointR (x, y) = (y, - x)

moveWaypoint :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveWaypoint [] _ pos = pos
moveWaypoint (i : ins) (wx, wy) (x, y) = moveWaypoint ins newWaypoint pos
  where
    (d, val) = parseCmd i
    rotations = val `div` 90
    newWaypoint
      | d == 'L' = iterate rotateWaypointL (wx, wy) !! rotations
      | d == 'R' = iterate rotateWaypointR (wx, wy) !! rotations
      | d == 'F' = (wx, wy)
      | otherwise = move (d, wx, wy) val
    pos
      | d == 'F' = (x + (wx * val), y + (wy * val))
      | otherwise = (x, y)

calcDistance :: (Int, Int) -> (Int, Int) -> Int
calcDistance (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

parseCmd :: String -> (Char, Int)
parseCmd cmd = (d, val)
  where
    [[_, [d], v]] = cmd =~ "([A-Z])([0-9]+)" :: [[String]]
    val = read v :: Int

main :: IO ()
main = do
  lines <- fmap lines (readFile "input.txt")
  let (_, x, y) = moveShip lines ('E', 0, 0)
  print $ calcDistance (0, 0) (x, y)

  let (x2, y2) = moveWaypoint lines (10, 1) (0, 0)
  print $ calcDistance (0, 0) (x2, y2)
