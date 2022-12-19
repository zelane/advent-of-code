module Day14 where

import Data.Either (fromRight)
import Data.HashSet qualified as S
import Data.List (foldl', sort)
import Text.Parsec (char, digit, many1, parse, sepBy, string)
import Text.Parsec.String (Parser)

type Point = (Int, Int)

type Walls = S.HashSet Point

drawLines :: Point -> [Point] -> Walls
drawLines _ [] = S.empty
drawLines a (b : ps) = points <> drawLines b ps
  where
    [ax, bx] = sort [fst a, fst b]
    [ay, by] = sort [snd a, snd b]
    points = S.fromList [(px, py) | px <- [ax .. bx], py <- [ay .. by]]

parseLine :: String -> [Point]
parseLine s = fromRight [] $ parse (sepBy xy (string " -> ")) "" s
  where
    xy = (\[a, b] -> (read a, read b)) <$> sepBy (many1 digit) (char ',')

stepSand :: Walls -> Point -> Point
stepSand walls (x, y)
  | isEmpty (x, y + 1) = (x, y + 1)
  | isEmpty (x - 1, y + 1) = (x - 1, y + 1)
  | isEmpty (x + 1, y + 1) = (x + 1, y + 1)
  | otherwise = (x, y)
  where
    isEmpty p = not $ S.member p walls

run :: (Point -> Bool) -> Int -> Point -> Walls -> Walls
run end maxy sand walls = if end nextSand then newWalls else run end maxy newSand newWalls
  where
    nextSand = stepSand walls sand
    sandStopped = nextSand == sand || snd sand == maxy
    (newSand, newWalls)
      | sandStopped = ((500, 0), S.insert sand walls)
      | otherwise = (nextSand, walls)

countS :: Walls -> Walls -> Int
countS start end = length $ S.difference end start

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let points = parseLine <$> input
  let walls = foldl' (\a l -> a <> drawLines (head l) (tail l)) S.empty points
  let maxY = maximum $ snd <$> concat points
  print $ countS walls $ run (\(x, y) -> y >= maxY) maxY (500, 0) walls
  print $ countS walls $ run (== (500, 0)) (maxY + 1) (500, 0) walls
