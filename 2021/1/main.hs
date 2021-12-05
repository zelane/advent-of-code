import Data.List (tails)

slide :: [Int] -> Int
slide [] = 0
slide [x] = 0
slide (x : xs) = fromEnum (head xs > x) + slide xs

main :: IO ()
main = do
  lines <- lines <$> readFile "test.txt"
  let ints = read <$> lines
  print $ slide ints
  print $ slide $ map (sum . take 3) (tails ints)
