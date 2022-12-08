import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let elves = splitOn [""] lines
      sums = reverse $ sort $ sum . fmap read <$> elves
  print $ head sums
  print $ sum $ take 3 sums
