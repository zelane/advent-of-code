import Data.List (sort)

answer1 :: [Int] -> (Int, Int) -> Int
answer1 [_] (a, b) = a * b
answer1 (x : xs) (a, b)
  | diff == 1 = answer1 xs (a + 1, b)
  | diff == 3 = answer1 xs (a, b + 1)
  where
    diff = head xs - x

answer2 :: [Int] -> Int -> Int
answer2 _ 0 = 1
answer2 points point
  | point `notElem` points = 0
  | otherwise = childPaths
  where
    childPaths = sum [answer2 points (point - x) | x <- [1 .. 3]]

main :: IO ()
main = do
  lines <- fmap lines (readFile "input.txt")
  let numbers = sort $ map read lines :: [Int]
  let chargers = [0] ++ numbers ++ [maximum numbers + 3]
  print $ answer1 chargers (0, 0)
  print $ answer2 chargers (maximum chargers)
