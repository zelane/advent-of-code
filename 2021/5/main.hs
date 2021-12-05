import Data.List (group, permutations, sort)
import Text.Regex.Posix ((=~))

type Line = ((Int, Int), (Int, Int))

parse :: String -> Line
parse s = ((a1, b1), (a2, b2))
  where
    [a1, b1, a2, b2] = map read $ drop 1 $ head (s =~ "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)" :: [[String]])

path :: Line -> [(Int, Int)]
path ((x1, y1), (x2, y2))
  | x1 == x2 || y1 == y2 = [(x, y) | x <- range x1 x2, y <- range y1 y2]
  | otherwise = zip (range x1 x2) (range y1 y2)
  where
    range a b = if a > b then reverse [b .. a] else [a .. b]

count :: [(Int, Int)] -> Int
count points = length $ filter ((> 1) . length) $ group $ sort points

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let parts = map parse lines

  print $ count $ concatMap path $ filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) parts

  -- let grid = [concat [if c > 0 then show c else "." | x <- [0 .. 10], let c = length $ filter (== (x, y)) lines] | y <- [0 .. 10]]
  -- mapM_ print grid

  print $ count $ concatMap path parts
