module Day17 where

import Data.Foldable (maximumBy, minimumBy)
import Data.List (elemIndex, scanl')
import Data.Ord (comparing)
import Data.Set qualified as S
import Debug.Trace (traceShow)

type Point = (Int, Int)

type Shape = [Point]

type Walls = S.Set Point

data State = State
  { idx :: Int,
    shape :: Shape,
    walls :: Walls,
    stopped :: Int
  }
  deriving (Show)

data Bound = Bound
  { top :: Int,
    right :: Int,
    bottom :: Int,
    left :: Int
  }
  deriving (Show)

jets :: String
jets = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

lineH :: Shape
lineH = [(0, 0), (1, 0), (2, 0), (3, 0)]

cross :: Shape
cross = [(0, 0), (1, 1), (-1, 1), (0, 1), (0, 2)]

shapeL :: Shape
shapeL = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]

lineV :: Shape
lineV = [(0, 0), (0, 1), (0, 2), (0, 3)]

square :: Shape
square = [(0, 0), (1, 0), (0, 1), (1, 1)]

order :: [Shape]
order = [lineH, cross, shapeL, lineV, square]

startingOffset :: [Int]
startingOffset = [2, 3, 2, 2, 2]

nextShape :: State -> State
nextShape state = state {idx = newIdx, shape = offset}
  where
    offset = applyT (startingOffset !! newIdx, maxy + 4) <$> newShape
    newShape = order !! newIdx
    newIdx = (idx state + 1) `mod` 5
    maxy
      | null $ walls state = 0
      | otherwise = maximum $ S.map snd (walls state)

applyT :: Point -> Point -> Point
applyT (tx, ty) (x, y) = (x + tx, y + ty)

bounds :: Shape -> Bound
bounds s = Bound miny maxx maxy minx
  where
    (minx, _) = minimumBy (comparing fst) s
    (maxx, _) = maximumBy (comparing fst) s
    (_, miny) = minimumBy (comparing snd) s
    (_, maxy) = maximumBy (comparing snd) s

collision :: Walls -> Shape -> (Bool, Bool)
collision walls s = (hcol, vcol || wallCol)
  where
    bound = bounds s
    hcol = left bound < 0 || right bound > 6
    vcol = bottom bound == 0
    wallCol = any (`elem` walls) s

blow :: Walls -> Char -> Shape -> (Bool, Shape)
blow walls ins shape
  | stopped = (False, shape) -- changed
  | not oob = (False, moved)
  | otherwise = (False, shape)
  where
    trans = if ins == '>' then (1, 0) else (-1, 0)
    moved = applyT trans <$> shape
    (oob, stopped) = collision walls moved

step :: State -> Char -> State
step state ins
  | stop1 = nextShape state {walls = newWallsBlow, stopped = stopped state + 1}
  | stop2 = nextShape state {walls = newWallsDrop, stopped = stopped state + 1}
  | otherwise = state {shape = dropp}
  where
    shape' = shape state
    (stop1, newShape) = blow (walls state) ins shape'
    dropp = applyT (0, -1) <$> newShape
    (_, stop2) = collision (walls state) dropp
    newWallsBlow = S.union (walls state) $ S.fromList shape' -- convert shape positions to walls
    newWallsDrop = S.union (walls state) $ S.fromList newShape -- convert shape positions to walls

draw :: State -> [String]
draw s = [[sprite (x, y) | x <- [0 .. 6]] | y <- reverse [0 .. maxy + 7]]
  where
    maxy = if null $ walls s then 10 else maximum $ S.map snd (walls s)
    sprite (x, y)
      | y == 0 = '_'
      | (x, y) `elem` walls s = '#'
      | (x, y) `elem` shape s = '@'
      | otherwise = '.'

solve :: IO String -> IO ()
solve file = do
  input <- file

  let start = nextShape $ State (-1) lineH S.empty 0
  let steps = scanl' step start (cycle input)
  -- mapM_ print $ concat $ draw <$> (take 2 $ drop 2022 steps)
  print $ maximum $ S.map snd $ walls $ (head $ dropWhile (\s -> stopped s < 2022) steps)
  print $ maximum $ S.map snd $ walls $ (head $ dropWhile (\s -> stopped s < 1000000000000) steps)

-- mapM_ print $ draw (head $ dropWhile (\s -> stopped s < 10) steps)

-- Don't have to save full shapes, just the max y value for each x
-- Or loop for a pattern? Does the pattern cycle match with new shapes?
