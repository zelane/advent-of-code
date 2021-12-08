import Data.List (group, sort, sortBy, sortOn, transpose)
import Data.Ord (comparing)

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let trans = map (sortOn length . group . sort) (transpose lines)
  print $ map (head . last) trans
  print $ map (head . head) trans
