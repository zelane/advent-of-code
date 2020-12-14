import Data.Set (singleton, Set, insert, union)

move :: [Char] -> (Int, Int) -> Set(Int, Int) -> Set(Int, Int)
move [] _ results = results
move (l:ls) (x, y) results = move ls newLoc (insert newLoc results)
      where newLoc | l == '^' = (x, y+1)
                   | l == 'v' = (x, y-1)
                   | l == '>' = (x+1, y)
                   | l == '<' = (x-1, y)

run :: [Char] -> Set(Int, Int)
run moves = move moves (0, 0) (singleton (0, 0))

filterIndex :: (Int -> Bool) -> [a] -> [a]
filterIndex f list = map snd $ filter (f.fst) indexed
      where indexed = zip [0..] list

splitOnIndex :: (Int -> Bool) -> [a] -> ([a], [a])
splitOnIndex f list = (filterIndex f list, filterIndex (not.f) list)

main :: IO ()
main = do
      moves <- readFile "input.txt"
      print $ length $ run moves

      let (santa, robo) = splitOnIndex odd moves
      print $ length $ union (run santa) (run robo)
