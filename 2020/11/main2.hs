import Data.List (takeWhile)
import Debug.Trace (traceShow)

trans :: [(Int, Int)]
trans = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, -1), (-1, 1), (1, 1), (-1, -1)]

applyTrans :: (Int, Int) -> (Int, Int) -> (Int, Int)
applyTrans (a, b) (c, d) = (a + c, b + d)

checkEyeLine :: [String] -> (Int, Int) -> (Int, Int) -> Bool
checkEyeLine seats (x, y) trans = '#' `elem` visibleSeats
  where
    maxX = length $ head seats
    maxY = length seats
    line = takeWhile valid $ drop 1 $ iterate (applyTrans trans) (x, y)
    valid (a, b) = a >= 0 && b >= 0 && a < maxX && b < maxY && getSeat (a, b) /= 'L'
    visibleSeats = map getSeat line
    getSeat (x, y) = (seats !! y) !! x

checkSeat :: [String] -> (Char, Int, Int) -> Char
checkSeat seats (state, x, y) = newState
  where
    occupied = length $ filter (checkEyeLine seats (x, y)) trans
    newState
      | state == 'L' && occupied == 0 = '#'
      | state == '#' && occupied >= 5 = 'L'
      | otherwise = state

step :: [String] -> [String]
step rows = [[checkSeat rows (c, x, y) | (x, c) <- zip [0 ..] row] | (y, row) <- zip [0 ..] rows]

run :: [String] -> Int
run oldState = if oldState == newState then result else run newState
  where
    newState = step oldState
    result = length $ filter (== '#') (concat newState)

main :: IO ()
main = do
  lines <- fmap lines (readFile "input.txt")
  print $ run lines
