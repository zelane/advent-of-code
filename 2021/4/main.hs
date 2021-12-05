import Data.List (inits, intersect, maximumBy, minimumBy, transpose, (\\))
import Data.List.Split (splitOn)
import Data.Ord (comparing)

type Board = [[Int]]

parseBoard :: [String] -> Board
parseBoard = map (map read . filter (/= "") . splitOn " ")

check :: [Int] -> Board -> Int
check calls b = if win then score else 0
  where
    rowMatches = map (intersect calls) (b ++ transpose b)
    win = any (\x -> length x == 5) rowMatches
    unmarked = concat b \\ calls
    score = sum unmarked * last calls

findWin :: [Int] -> Board -> (Int, Int)
findWin calls b = (length turns, head scores)
  where
    (turns, scores) = break (> 0) [check call b | call <- inits calls]

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let calls = map read $ splitOn "," $ head lines :: [Int]
  let boards = map parseBoard $ splitOn [""] $ drop 2 lines

  let wins = map (findWin calls) boards
  print $ snd $ minimumBy (comparing fst) wins
  print $ snd $ maximumBy (comparing fst) wins
