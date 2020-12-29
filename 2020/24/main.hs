import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

trans :: Map String (Int, Int)
trans = M.fromList [("e", (2, 0)), ("se", (1, -2)), ("sw", (-1, -2)), ("w", (-2, 0)), ("nw", (-1, 2)), ("ne", (1, 2))]

parseDirs :: [String] -> String -> [String]
parseDirs acc [] = acc
parseDirs acc [x] = acc ++ [[x]]
parseDirs acc dirs
  | ab `elem` ["se", "sw", "ne", "nw"] = acc ++ [ab] ++ parseDirs acc (drop 2 dirs)
  | otherwise = acc ++ [[a]] ++ parseDirs acc (drop 1 dirs)
  where
    ab = take 2 dirs
    a = head ab

move :: (Int, Int) -> String -> Map (Int, Int) Char -> ((Int, Int), Map (Int, Int) Char)
move (x, y) dir floor = (newCoords, newFloor)
  where
    (tx, ty) = fromJust $ M.lookup dir trans
    newCoords = (x + tx, y + ty)
    tile = M.findWithDefault 'W' newCoords floor
    newFloor = M.insert newCoords tile floor

plan :: (Int, Int) -> Map (Int, Int) Char -> [String] -> Map (Int, Int) Char
plan coords floor [] = M.insert coords newTile floor
  where
    tile = fromJust $ M.lookup coords floor
    newTile = if tile == 'B' then 'W' else 'B'
plan coords floor (i : ins) = plan newCoords newFloor ins
  where
    (newCoords, newFloor) = move coords i floor

run :: [[String]] -> Map (Int, Int) Char -> Map (Int, Int) Char
run [] floor = floor
run (i : ins) floor = run ins done
  where
    done = plan (0, 0) floor i

toFlip :: Map (Int, Int) Char -> (Int, Int) -> Char -> Char
toFlip floor (x, y) t = newT
  where
    adjCoords = [(x + tx, y + ty) | (tx, ty) <- M.elems trans]
    adj = [M.findWithDefault 'W' (x, y) floor | (x, y) <- adjCoords]
    black = length $ filter (== 'B') adj
    newT
      | t == 'B' && (black == 0 || black > 2) = 'W'
      | t == 'W' && black == 2 = 'B'
      | otherwise = t

fill' :: Map (Int, Int) Char -> [(Int, Int)] -> Map (Int, Int) Char
fill' floor [] = floor
fill' floor (c : coords) = fill' newFloor coords
  where
    newFloor = if M.member c floor then floor else M.insert c 'W' floor

fill :: Map (Int, Int) Char -> (Int, Int) -> Char -> (Map (Int, Int) Char, Char)
fill floor (x, y) c = (newFloor, c)
  where
    adjCoords = [(x + tx, y + ty) | (tx, ty) <- M.elems trans]
    newFloor = fill' floor adjCoords

run2 :: Int -> Map (Int, Int) Char -> [Int] -> [Int]
run2 0 floor acc = acc
run2 runs floor acc = run2 (runs -1) flipped (acc ++ [c])
  where
    (filled, _) = M.mapAccumWithKey fill floor floor
    flipped = M.mapWithKey (toFlip filled) filled
    c = length $ filter ((== 'B') . snd) $ M.toList flipped

main :: IO ()
main = do
  lines <- fmap lines (readFile "input.txt")

  let ins = map (parseDirs []) lines
  let floor = M.fromList [((0, 0), 'W')]
  let decorated = run ins floor
  print $ length $ filter ((== 'B') . snd) $ M.toList decorated
  print $ last $ run2 100 decorated []
