{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (iterate')
import Data.List.Split (splitOn)

type Instruction = (Char, Int)

type Position = (Int, Int, Int)

directions :: [(Int, Int)]
directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]

parseIns :: String -> Instruction
parseIns (s : sx) = (s, read sx)

applyT :: (Int, Int) -> Position -> Position
applyT (mx, my) (x, y, d) = (x + mx, y + my, d)

applyIns :: Position -> Instruction -> [Position]
applyIns (x, y, d) (dir, dist) = take dist $ drop 1 $ iterate' (applyT (modX, modY)) (x, y, newDir)
  where
    (modX, modY) = directions !! newDir
    newDir = (if dir == 'R' then d + 1 else d + 3) `mod` 4

findDupe :: [Position] -> [(Int, Int)] -> (Int, Int)
findDupe ((x, y, _) : ps) visited =
  if (x, y) `elem` visited
    then (x, y)
    else findDupe ps (visited ++ [(x, y)])

main :: IO ()
main = do
  words <- splitOn ", " <$> readFile "input.txt"
  let ins = parseIns <$> words
  let path = foldl (\a x -> a ++ applyIns (last a) x) [(0, 0, 0)] ins
  let (x, y, _) = last path
  print $ abs x + abs y
  let (dx, dy) = findDupe path []
  print $ abs dx + abs dy
