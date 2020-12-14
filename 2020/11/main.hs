import Debug.Trace

trans :: [(Int, Int)]
trans = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, -1), (-1, 1), (1, 1), (-1, -1)]

applyTrans :: (Int, Int) -> (Int, Int) -> (Int, Int)
applyTrans (a, b) (c, d) = (a + c, b + d)

getOccupied :: [String] -> (Int, Int) -> Int
getOccupied seats (x, y) = length $ filter (== '#') adj
  where
    maxX = length $ head seats
    maxY = length seats
    adj = [(seats !! yt) !! xt | (xt, yt) <- adjCoords, xt >= 0 && yt >= 0 && xt < maxX && yt < maxY]
    adjCoords = map (applyTrans (x, y)) trans

checkSeat :: [String] -> (Char, Int, Int) -> Char
checkSeat seats (state, x, y) = newState
  where
    occupied = getOccupied seats (x, y)
    newState
      | state == 'L' && occupied == 0 = '#'
      | state == '#' && occupied >= 4 = 'L'
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
